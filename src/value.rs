//! Definition of a TOML value

use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::hash::Hash;
use std::mem::discriminant;
use std::ops;

use serde::de;
use serde::ser;

use crate::datetime::{self, DatetimeFromString};
pub use crate::datetime::{Date, Datetime, DatetimeParseError, Offset, Time};

pub use crate::map::{Entry, Map};

/// Representation of a TOML value.
#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    /// Represents a TOML string
    String(String),
    /// Represents a TOML integer
    Integer(i64),
    /// Represents a TOML float
    Float(f64),
    /// Represents a TOML boolean
    Boolean(bool),
    /// Represents a TOML datetime
    Datetime(Datetime),
    /// Represents a TOML array
    Array(Array),
    /// Represents a TOML table
    Table(Table),
}

/// Type representing a TOML array, payload of the `Value::Array` variant
pub type Array = Vec<Value>;

/// Type representing a TOML table, payload of the `Value::Table` variant.
/// By default it is backed by a BTreeMap, enable the `preserve_order` feature
/// to use a LinkedHashMap instead.
pub type Table = Map<String, Value>;

impl Value {
    /// Index into a TOML array or map. A string index can be used to access a
    /// value in a map, and a usize index can be used to access an element of an
    /// array.
    ///
    /// Returns `None` if the type of `self` does not match the type of the
    /// index, for example if the index is a string and `self` is an array or a
    /// number. Also returns `None` if the given key does not exist in the map
    /// or the given index is not within the bounds of the array.
    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index(self)
    }

    /// Mutably index into a TOML array or map. A string index can be used to
    /// access a value in a map, and a usize index can be used to access an
    /// element of an array.
    ///
    /// Returns `None` if the type of `self` does not match the type of the
    /// index, for example if the index is a string and `self` is an array or a
    /// number. Also returns `None` if the given key does not exist in the map
    /// or the given index is not within the bounds of the array.
    pub fn get_mut<I: Index>(&mut self, index: I) -> Option<&mut Value> {
        index.index_mut(self)
    }

    /// Extracts the integer value if it is an integer.
    pub fn as_integer(&self) -> Option<i64> {
        match *self {
            Value::Integer(i) => Some(i),
            _ => None,
        }
    }

    /// Tests whether this value is an integer.
    pub fn is_integer(&self) -> bool {
        self.as_integer().is_some()
    }

    /// Extracts the float value if it is a float.
    pub fn as_float(&self) -> Option<f64> {
        match *self {
            Value::Float(f) => Some(f),
            _ => None,
        }
    }

    /// Tests whether this value is a float.
    pub fn is_float(&self) -> bool {
        self.as_float().is_some()
    }

    /// Extracts the boolean value if it is a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            Value::Boolean(b) => Some(b),
            _ => None,
        }
    }

    /// Tests whether this value is a boolean.
    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    /// Extracts the string of this value if it is a string.
    pub fn as_str(&self) -> Option<&str> {
        match *self {
            Value::String(ref s) => Some(&**s),
            _ => None,
        }
    }

    /// Tests if this value is a string.
    pub fn is_str(&self) -> bool {
        self.as_str().is_some()
    }

    /// Extracts the datetime value if it is a datetime.
    ///
    /// Note that a parsed TOML value will only contain ISO 8601 dates. An
    /// example date is:
    ///
    /// ```notrust
    /// 1979-05-27T07:32:00Z
    /// ```
    pub fn as_datetime(&self) -> Option<&Datetime> {
        match *self {
            Value::Datetime(ref s) => Some(s),
            _ => None,
        }
    }

    /// Tests whether this value is a datetime.
    pub fn is_datetime(&self) -> bool {
        self.as_datetime().is_some()
    }

    /// Extracts the array value if it is an array.
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match *self {
            Value::Array(ref s) => Some(s),
            _ => None,
        }
    }

    /// Extracts the array value if it is an array.
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match *self {
            Value::Array(ref mut s) => Some(s),
            _ => None,
        }
    }

    /// Tests whether this value is an array.
    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    /// Extracts the table value if it is a table.
    pub fn as_table(&self) -> Option<&Table> {
        match *self {
            Value::Table(ref s) => Some(s),
            _ => None,
        }
    }

    /// Extracts the table value if it is a table.
    pub fn as_table_mut(&mut self) -> Option<&mut Table> {
        match *self {
            Value::Table(ref mut s) => Some(s),
            _ => None,
        }
    }

    /// Tests whether this value is a table.
    pub fn is_table(&self) -> bool {
        self.as_table().is_some()
    }

    /// Tests whether this and another value have the same type.
    pub fn same_type(&self, other: &Value) -> bool {
        discriminant(self) == discriminant(other)
    }

    /// Returns a human-readable representation of the type of this value.
    pub fn type_str(&self) -> &'static str {
        match *self {
            Value::String(..) => "string",
            Value::Integer(..) => "integer",
            Value::Float(..) => "float",
            Value::Boolean(..) => "boolean",
            Value::Datetime(..) => "datetime",
            Value::Array(..) => "array",
            Value::Table(..) => "table",
        }
    }
}

impl<I> ops::Index<I> for Value
where
    I: Index,
{
    type Output = Value;

    fn index(&self, index: I) -> &Value {
        self.get(index).expect("index not found")
    }
}

impl<I> ops::IndexMut<I> for Value
where
    I: Index,
{
    fn index_mut(&mut self, index: I) -> &mut Value {
        self.get_mut(index).expect("index not found")
    }
}

impl<'a> From<&'a str> for Value {
    #[inline]
    fn from(val: &'a str) -> Value {
        Value::String(val.to_string())
    }
}

impl<V: Into<Value>> From<Vec<V>> for Value {
    fn from(val: Vec<V>) -> Value {
        Value::Array(val.into_iter().map(|v| v.into()).collect())
    }
}

impl<S: Into<String>, V: Into<Value>> From<BTreeMap<S, V>> for Value {
    fn from(val: BTreeMap<S, V>) -> Value {
        let table = val.into_iter().map(|(s, v)| (s.into(), v.into())).collect();

        Value::Table(table)
    }
}

impl<S: Into<String> + Hash + Eq, V: Into<Value>> From<HashMap<S, V>> for Value {
    fn from(val: HashMap<S, V>) -> Value {
        let table = val.into_iter().map(|(s, v)| (s.into(), v.into())).collect();

        Value::Table(table)
    }
}

macro_rules! impl_into_value {
    ($variant:ident : $T:ty) => {
        impl From<$T> for Value {
            #[inline]
            fn from(val: $T) -> Value {
                Value::$variant(val.into())
            }
        }
    };
}

impl_into_value!(String: String);
impl_into_value!(Integer: i64);
impl_into_value!(Integer: i32);
impl_into_value!(Integer: i8);
impl_into_value!(Integer: u8);
impl_into_value!(Integer: u32);
impl_into_value!(Float: f64);
impl_into_value!(Float: f32);
impl_into_value!(Boolean: bool);
impl_into_value!(Datetime: Datetime);
impl_into_value!(Table: Table);

/// Types that can be used to index a `toml::Value`
///
/// Currently this is implemented for `usize` to index arrays and `str` to index
/// tables.
///
/// This trait is sealed and not intended for implementation outside of the
/// `toml` crate.
pub trait Index: Sealed {
    #[doc(hidden)]
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value>;
    #[doc(hidden)]
    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value>;
}

/// An implementation detail that should not be implemented, this will change in
/// the future and break code otherwise.
#[doc(hidden)]
pub trait Sealed {}
impl Sealed for usize {}
impl Sealed for str {}
impl Sealed for String {}
impl<'a, T: Sealed + ?Sized> Sealed for &'a T {}

impl Index for usize {
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value> {
        match *val {
            Value::Array(ref a) => a.get(*self),
            _ => None,
        }
    }

    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value> {
        match *val {
            Value::Array(ref mut a) => a.get_mut(*self),
            _ => None,
        }
    }
}

impl Index for str {
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value> {
        match *val {
            Value::Table(ref a) => a.get(self),
            _ => None,
        }
    }

    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value> {
        match *val {
            Value::Table(ref mut a) => a.get_mut(self),
            _ => None,
        }
    }
}

impl Index for String {
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value> {
        self[..].index(val)
    }

    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value> {
        self[..].index_mut(val)
    }
}

impl<'s, T: ?Sized> Index for &'s T
where
    T: Index,
{
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value> {
        (**self).index(val)
    }

    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value> {
        (**self).index_mut(val)
    }
}

impl ser::Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        use serde::ser::SerializeMap;

        match *self {
            Value::String(ref s) => serializer.serialize_str(s),
            Value::Integer(i) => serializer.serialize_i64(i),
            Value::Float(f) => serializer.serialize_f64(f),
            Value::Boolean(b) => serializer.serialize_bool(b),
            Value::Datetime(ref s) => s.serialize(serializer),
            Value::Array(ref a) => a.serialize(serializer),
            Value::Table(ref t) => {
                let mut map = serializer.serialize_map(Some(t.len()))?;
                // Be sure to visit non-tables first (and also non
                // array-of-tables) as all keys must be emitted first.
                for (k, v) in t {
                    if !v.is_table() && !v.is_array()
                        || (v
                            .as_array()
                            .map(|a| !a.iter().any(|v| v.is_table()))
                            .unwrap_or(false))
                    {
                        map.serialize_entry(k, v)?;
                    }
                }
                for (k, v) in t {
                    if v.as_array()
                        .map(|a| a.iter().any(|v| v.is_table()))
                        .unwrap_or(false)
                    {
                        map.serialize_entry(k, v)?;
                    }
                }
                for (k, v) in t {
                    if v.is_table() {
                        map.serialize_entry(k, v)?;
                    }
                }
                map.end()
            }
        }
    }
}

impl<'de> de::Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> de::Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("any valid TOML value")
            }

            fn visit_bool<E>(self, value: bool) -> Result<Value, E> {
                Ok(Value::Boolean(value))
            }

            fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
                Ok(Value::Integer(value))
            }

            fn visit_u64<E: de::Error>(self, value: u64) -> Result<Value, E> {
                if value <= i64::max_value() as u64 {
                    Ok(Value::Integer(value as i64))
                } else {
                    Err(de::Error::custom("u64 value was too large"))
                }
            }

            fn visit_u32<E>(self, value: u32) -> Result<Value, E> {
                Ok(Value::Integer(value.into()))
            }

            fn visit_i32<E>(self, value: i32) -> Result<Value, E> {
                Ok(Value::Integer(value.into()))
            }

            fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
                Ok(Value::Float(value))
            }

            fn visit_str<E>(self, value: &str) -> Result<Value, E> {
                Ok(Value::String(value.into()))
            }

            fn visit_string<E>(self, value: String) -> Result<Value, E> {
                Ok(Value::String(value))
            }

            fn visit_some<D>(self, deserializer: D) -> Result<Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                de::Deserialize::deserialize(deserializer)
            }

            fn visit_seq<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: de::SeqAccess<'de>,
            {
                let mut vec = Vec::new();
                while let Some(elem) = visitor.next_element()? {
                    vec.push(elem);
                }
                Ok(Value::Array(vec))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut key = String::new();
                let datetime = visitor.next_key_seed(DatetimeOrTable { key: &mut key })?;
                match datetime {
                    Some(true) => {
                        let date: DatetimeFromString = visitor.next_value()?;
                        return Ok(Value::Datetime(date.value));
                    }
                    None => return Ok(Value::Table(Map::new())),
                    Some(false) => {}
                }
                let mut map = Map::new();
                map.insert(key, visitor.next_value()?);
                while let Some(key) = visitor.next_key::<String>()? {
                    if let Entry::Vacant(vacant) = map.entry(&key) {
                        vacant.insert(visitor.next_value()?);
                    } else {
                        let msg = format!("duplicate key: `{}`", key);
                        return Err(de::Error::custom(msg));
                    }
                }
                Ok(Value::Table(map))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

struct DatetimeOrTable<'a> {
    key: &'a mut String,
}

impl<'a, 'de> de::DeserializeSeed<'de> for DatetimeOrTable<'a> {
    type Value = bool;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'a, 'de> de::Visitor<'de> for DatetimeOrTable<'a> {
    type Value = bool;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("a string key")
    }

    fn visit_str<E>(self, s: &str) -> Result<bool, E>
    where
        E: de::Error,
    {
        if s == datetime::FIELD {
            Ok(true)
        } else {
            self.key.push_str(s);
            Ok(false)
        }
    }

    fn visit_string<E>(self, s: String) -> Result<bool, E>
    where
        E: de::Error,
    {
        if s == datetime::FIELD {
            Ok(true)
        } else {
            *self.key = s;
            Ok(false)
        }
    }
}
