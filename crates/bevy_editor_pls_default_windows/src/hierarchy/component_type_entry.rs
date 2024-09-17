use bevy::ecs::component::ComponentId;

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct ComponentTypeEntry {
    pub short_name: String,
    pub full_path: String,
    pub is_ambiguous: bool,
    pub component_id: Option<ComponentId>,
}
