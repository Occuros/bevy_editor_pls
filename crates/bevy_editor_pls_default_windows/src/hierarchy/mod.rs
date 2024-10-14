// pub mod picking;

mod component_type_entry;
mod egui_autocomplete;
mod filter_node;

use bevy::ecs::entity::Entities;
use bevy::ecs::world::FilteredEntityRef;
use bevy::pbr::wireframe::Wireframe;
use bevy::prelude::*;
use bevy::reflect::TypeRegistry;
use bevy::render::{Extract, RenderApp};
use bevy::utils::HashSet;
use bevy_editor_pls_core::{
    editor_window::{EditorWindow, EditorWindowContext},
    Editor,
};
use bevy_inspector_egui::bevy_inspector::guess_entity_name;
use bevy_inspector_egui::bevy_inspector::hierarchy::SelectedEntities;
use bevy_inspector_egui::egui::text::CCursorRange;
use bevy_inspector_egui::egui::{self, Key, ScrollArea};
use std::any::{Any, TypeId};
use std::collections::{BTreeSet, HashMap};

use crate::add::{add_ui, AddWindow, AddWindowState};
use crate::debug_settings::DebugSettingsWindow;
use crate::hierarchy::component_type_entry::ComponentTypeEntry;
use crate::hierarchy::egui_autocomplete::AutoCompleteTextEdit;
use crate::hierarchy::filter_node::FilterNode;
use crate::inspector::{InspectorSelection, InspectorWindow};

#[derive(Component)]
pub struct HideInEditor;

pub struct HierarchyWindow;
impl EditorWindow for HierarchyWindow {
    type State = HierarchyState;
    const NAME: &'static str = "Hierarchy";

    fn ui(world: &mut World, mut cx: EditorWindowContext, ui: &mut egui::Ui) {
        let (hierarchy_state, inspector_state, add_state) =
            match cx.state_mut_triplet::<HierarchyWindow, InspectorWindow, AddWindow>() {
                Some((a, b, c)) => (a, b, Some(c)),
                None => {
                    let (a, b) = cx
                        .state_mut_pair::<HierarchyWindow, InspectorWindow>()
                        .unwrap();
                    (a, b, None)
                }
            };

        initialize_filter_state_if_needed(&mut hierarchy_state.filter_state, world);

        ScrollArea::vertical().show(ui, |ui| {
            let type_registry = world.resource::<AppTypeRegistry>().clone();
            let type_registry = type_registry.read();
            let new_selected = Hierarchy {
                world,
                state: hierarchy_state,
                type_registry: &type_registry,
                add_state: add_state.as_deref(),
            }
            .show(ui);

            if new_selected {
                inspector_state.selected = InspectorSelection::Entities;
            }
        });
    }

    fn app_setup(app: &mut bevy::prelude::App) {
        // picking::setup(app);
        app.add_systems(PostUpdate, clear_removed_entites);
        // .add_system(handle_events);

        app.sub_app_mut(RenderApp)
            .add_systems(ExtractSchedule, extract_wireframe_for_selected);
    }
}

fn clear_removed_entites(mut editor: ResMut<Editor>, entities: &Entities) {
    let state = editor.window_state_mut::<HierarchyWindow>().unwrap();
    state.selected.retain(|entity| entities.contains(entity));
}

/*fn handle_events(
    mut click_events: EventReader<PointerClick>,
    mut editor: ResMut<Editor>,
    editor_state: Res<EditorState>,
    input: Res<Input<KeyCode>>,
    egui_entity: Query<&EguiPointer>,
    mut egui_ctx: ResMut<EguiContext>,
) {
    for click in click_events.iter() {
        if !editor_state.active {
            return;
        }

        if click.event_data().button != PointerButton::Primary {
            continue;
        }

        if egui_entity.get(click.target()).is_ok() || egui_ctx.ctx_mut().wants_pointer_input() {
            continue;
        };

        let state = editor.window_state_mut::<HierarchyWindow>().unwrap();

        let ctrl = input.any_pressed([KeyCode::LControl, KeyCode::RControl]);
        let shift = input.any_pressed([KeyCode::LShift, KeyCode::RShift]);
        let mode = SelectionMode::from_ctrl_shift(ctrl, shift);

        let entity = click.target();
        info!("Selecting mesh, found {:?}", entity);
        state
            .selected
            .select(mode, entity, |_, _| std::iter::once(entity));
    }
}*/

fn extract_wireframe_for_selected(editor: Extract<Res<Editor>>, mut commands: Commands) {
    let wireframe_for_selected = editor
        .window_state::<DebugSettingsWindow>()
        .map_or(false, |settings| settings.highlight_selected);

    if wireframe_for_selected {
        let selected = &editor.window_state::<HierarchyWindow>().unwrap().selected;
        for selected in selected.iter() {
            commands.get_or_spawn(selected).insert(Wireframe);
        }
    }
}

#[derive(Default)]
pub struct HierarchyState {
    pub selected: SelectedEntities,
    rename_info: Option<RenameInfo>,
    filter_state: FilterState,
}

pub struct RenameInfo {
    entity: Entity,
    renaming: bool,
    current_rename: String,
}

#[derive(Default)]
pub struct FilterState {
    pub search_text: String,
    pub filters: Vec<FilterNode>,
    pub next_filter: Option<FilterNode>,
    //this can be cached? might require verification
    pub all_components: HashMap<String, ComponentTypeEntry>,
    pub request_focus_next_frame: bool,
    hide_in_editor_component: ComponentTypeEntry,
    parent_component: ComponentTypeEntry,
}

struct Hierarchy<'a> {
    world: &'a mut World,
    state: &'a mut HierarchyState,
    type_registry: &'a TypeRegistry,
    add_state: Option<&'a AddWindowState>,
}

impl<'a> Hierarchy<'a> {
    fn show(&mut self, ui: &mut egui::Ui) -> bool {
        let mut despawn_recursive = None;
        let mut despawn = None;

        let HierarchyState {
            selected,
            rename_info,
            filter_state,
        } = self.state;

        ui.with_layout(
            egui::Layout::left_to_right(egui::Align::TOP).with_main_wrap(true),
            |ui| {
                filter_state
                    .filters
                    .retain(|f| !ui.button(&f.short_form_with_value()).clicked())
            },
        );

        ui.horizontal(|ui| {
            if let Some(filter) = &filter_state.next_filter {
                if ui.button(filter.short_form()).clicked() {
                    filter_state.next_filter = None;
                }
            }

            if filter_state.next_filter.is_none() {
                handle_next_filter_selection(filter_state, ui);
            } else {
                handle_component_filter(filter_state, self.world, ui);
            }
        });

        let mut filters = filter_state.filters.clone();
        let index_fields = filters
            .iter()
            .filter(|f| matches!(f, FilterNode::Index(_)))
            .count();
        let only_index = index_fields > 0 && filters.iter().count() == index_fields;

        if filters.len() == 0 {
            filters.push(FilterNode::Without(filter_state.parent_component.clone()))
        }
        filters.push(FilterNode::Without(
            filter_state.hide_in_editor_component.clone(),
        ));

        let entities = gather_entities_from_filters(&filters, self.world, only_index);

        let new_selection = bevy_inspector_egui::bevy_inspector::hierarchy::Hierarchy {
            extra_state: rename_info,
            world: self.world,
            type_registry: self.type_registry,
            selected,
            context_menu: Some(&mut |ui, entity, world, rename_info| {
                if ui.button("Despawn").clicked() {
                    despawn_recursive = Some(entity);
                }

                if ui.button("Remove keeping children").clicked() {
                    despawn = Some(entity);
                }

                if ui.button("Rename").clicked() {
                    let entity_name = guess_entity_name(world, entity);
                    *rename_info = Some(RenameInfo {
                        entity,
                        renaming: true,
                        current_rename: entity_name,
                    });
                    ui.close_menu();
                }

                if let Some(add_state) = self.add_state {
                    ui.menu_button("Add", |ui| {
                        if let Some(add_item) = add_ui(ui, add_state) {
                            add_item.add_to_entity(world, entity);
                            ui.close_menu();
                        }
                    });
                }
            }),
            shortcircuit_entity: Some(&mut |ui, entity, world, rename_info| {
                if let Some(rename_info) = rename_info {
                    if rename_info.renaming && rename_info.entity == entity {
                        rename_entity_ui(ui, rename_info, world);

                        return true;
                    }
                }

                false
            }),
        }
        // .show::<Without<HideInEditor>>(ui);
        .show_with_query_state(ui, &mut entities.into_iter().collect());

        if let Some(entity) = despawn_recursive {
            bevy::hierarchy::despawn_with_children_recursive(self.world, entity);
        }
        if let Some(entity) = despawn {
            self.world.entity_mut(entity).despawn();
            self.state.selected.remove(entity);
        }

        if ui.input(|input| input.key_pressed(egui::Key::Delete)) {
            for entity in self.state.selected.iter() {
                self.world.entity_mut(entity).despawn_recursive();
            }
            self.state.selected.clear();
        }

        new_selection
    }
}

pub fn handle_next_filter_selection(state: &mut FilterState, ui: &mut egui::Ui) {
    let mut inputs = BTreeSet::new();

    for s in FilterNode::short_forms().iter() {
        inputs.insert(s.to_owned());
    }

    let auto_complete = AutoCompleteTextEdit::new(&mut state.search_text, inputs)
        .max_suggestions(5)
        .auto_select_first_entry(true)
        .highlight_matches(true);

    let response = ui.add(auto_complete);
    if state.request_focus_next_frame {
        response.request_focus();
        state.request_focus_next_frame = false;
    }
    let mut next_filter = None;

    let accepted_by_keyboard = ui.input_mut(|input| input.key_pressed(Key::Enter))
        || ui.input_mut(|input| input.key_pressed(Key::Tab));
    if accepted_by_keyboard || response.clicked() {
        next_filter = FilterNode::try_from(&state.search_text);
    }

    if next_filter.is_some() {
        state.next_filter = next_filter;
        state.search_text.clear();
        state.request_focus_next_frame = true;
    }
}

pub fn handle_component_filter(state: &mut FilterState, world: &mut World, ui: &mut egui::Ui) {
    let Some(filter) = state.next_filter.as_ref() else {
        return;
    };
    let mut inputs = BTreeSet::new();

    match filter {
        FilterNode::Index(_) => {
            for e in world.iter_entities() {
                inputs.insert(e.id().index().to_string());
            }
        }
        _ => {
            for c in state.all_components.values() {
                if c.is_ambiguous {
                    inputs.insert(c.full_path.clone());
                } else {
                    inputs.insert(c.short_name.clone());
                }
            }
        }
    }

    // let mut selected_option = None;
    let auto_complete = AutoCompleteTextEdit::new(&mut state.search_text, inputs)
        .max_suggestions(5)
        .auto_select_first_entry(true)
        .highlight_matches(true);

    let response = ui.add(auto_complete);

    if state.request_focus_next_frame {
        response.request_focus();
        state.request_focus_next_frame = false;
    }

    let accepted_by_keyboard = ui.input_mut(|input| input.key_pressed(Key::Enter))
        || ui.input_mut(|input| input.key_pressed(Key::Tab));

    if !state.search_text.is_empty() && (accepted_by_keyboard || response.clicked()) {
        match filter {
            FilterNode::Index(_) => {
                let Ok(index) = state.search_text.parse() else {
                    return;
                };
                state.filters.push(FilterNode::Index(index));
            }

            FilterNode::With(_) => {
                let Some(component) = state.all_components.get(&state.search_text) else {
                    return;
                };
                state.filters.push(FilterNode::With(component.clone()))
            }
            FilterNode::Without(_) => {
                let Some(component) = state.all_components.get(&state.search_text) else {
                    return;
                };
                state.filters.push(FilterNode::Without(component.clone()))
            }
        };
        state.next_filter = None;
        state.search_text.clear();
        state.request_focus_next_frame = true;
    }
}

fn initialize_filter_state_if_needed(state: &mut FilterState, world: &World) {
    let type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = type_registry.read();
    if state.all_components.is_empty() {
        for t in type_registry.iter() {
            let short_path = t.type_info().type_path_table().short_path();
            let full_path = t.type_info().type_path();
            let is_ambiguous = type_registry.is_ambiguous(short_path);
            let key = if is_ambiguous {
                t.type_info().type_path()
            } else {
                short_path
            }
            .to_owned();

            let component_id = type_registry
                .get_with_type_path(full_path)
                .map(|t| world.components().get_id(t.type_id()))
                .flatten();

            let component = ComponentTypeEntry {
                full_path: full_path.to_owned(),
                short_name: short_path.to_owned(),
                is_ambiguous,
                component_id,
            };
            if t.type_id() == HideInEditor.type_id() {
                state.hide_in_editor_component = component.clone();
            }

            if t.type_id() == TypeId::of::<Parent>() {
                state.parent_component = component.clone();
            }

            state.all_components.insert(key, component);
        }
    }
}

fn rename_entity_ui(ui: &mut egui::Ui, rename_info: &mut RenameInfo, world: &mut World) {
    use egui::epaint::text::cursor::CCursor;
    use egui::widgets::text_edit::{TextEdit, TextEditOutput};

    let id = egui::Id::new(rename_info.entity);

    let edit = TextEdit::singleline(&mut rename_info.current_rename).id(id);
    let TextEditOutput {
        response,
        state: mut edit_state,
        ..
    } = edit.show(ui);

    // Runs once to end renaming
    if response.lost_focus() {
        rename_info.renaming = false;

        match world.get_entity_mut(rename_info.entity) {
            Some(mut ent_mut) => match ent_mut.get_mut::<Name>() {
                Some(mut name) => {
                    name.set(rename_info.current_rename.clone());
                }
                None => {
                    ent_mut.insert(Name::new(rename_info.current_rename.clone()));
                }
            },
            None => {
                error!("Failed to get renamed entity");
            }
        }
    }

    // Runs once when renaming begins
    if !response.has_focus() {
        response.request_focus();
        edit_state.cursor.set_char_range(Some(CCursorRange::two(
            CCursor::new(0),
            CCursor::new(rename_info.current_rename.len()),
        )));
    }

    TextEdit::store_state(ui.ctx(), id, edit_state);
}

pub fn gather_entities_from_filters(
    filters: &Vec<FilterNode>,
    world: &mut World,
    only_indexed: bool,
) -> HashSet<Entity> {
    let mut query = QueryBuilder::<FilteredEntityRef>::new(world);
    let mut entities = HashSet::new();
    for filter in filters.iter() {
        match filter {
            FilterNode::Index(id) => {
                entities.insert(Entity::from_raw(*id));
            }
            FilterNode::With(component) => {
                let Some(component_id) = component.component_id else {
                    continue;
                };
                query.with_id(component_id);
            }
            FilterNode::Without(component) => {
                let Some(component_id) = component.component_id else {
                    continue;
                };
                query.without_id(component_id);
            }
        }
    }
    if !only_indexed {
        for entity_ref in query.build().iter(world) {
            entities.insert(entity_ref.id());
        }
    }

    entities
}
