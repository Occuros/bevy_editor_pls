use crate::hierarchy::component_type_entry::ComponentTypeEntry;
use crate::hierarchy::filter_node::FilterNode::*;

#[derive(Clone, Debug)]
pub enum FilterNode {
    Index(u32),
    With(ComponentTypeEntry),
    Without(ComponentTypeEntry),
}

impl FilterNode {
    pub fn short_form(&self) -> &str {
        match self {
            Index(_) => "i:",
            With(_) => "w:",
            Without(_) => "x:",
        }
    }

    pub fn short_form_with_value(&self) -> String {
        match self {
            Index(value) => format!("i:{}", value),
            With(value) => format!("w:{}", value.short_name),
            Without(value) => format!("x:{}", value.short_name),
        }
    }
    pub fn short_forms() -> [String; 3] {
        ["i:".to_owned(), "w:".to_owned(), "x:".to_owned()]
    }

    pub fn try_from(short_name: &str) -> Option<FilterNode> {
        let value = ComponentTypeEntry::default();
        match short_name {
            "i:" => Some(Index(0)),
            "w:" => Some(With(value)),
            "x:" => Some(Without(value)),
            _ => None,
        }
    }
}
