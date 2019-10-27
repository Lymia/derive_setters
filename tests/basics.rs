use derive_inherent_builder::*;

use std::borrow::Cow;

#[derive(Default, InherentBuilder, Debug, PartialEq, Eq)]
struct BasicStruct {
    #[builder(rename = "test")]
    a: u32,
    b: u32,
    c: u32,
}

#[test]
fn basic_struct() {
    assert_eq!(
        BasicStruct::default().test(30).b(10).c(20),
        BasicStruct { a: 30, b: 10, c: 20 },
    );
    assert_eq!(
        BasicStruct::default().b(15).test(10),
        BasicStruct { a: 10, b: 15, ..Default::default() },
    );
}

#[derive(Default, InherentBuilder, Debug, PartialEq, Eq)]
struct GenericStruct<'a, T> {
    a: Option<&'a T>,
    b: Option<&'a T>,
    c: T,
}

#[test]
fn generic_struct() {
    assert_eq!(
        GenericStruct::default().a(Some(&30)).b(Some(&10)).c(20),
        GenericStruct { a: Some(&30), b: Some(&10), c: 20 },
    );
    assert_eq!(
        GenericStruct::default().b(Some(&15)).a(Some(&10)),
        GenericStruct { a: Some(&10), b: Some(&15), ..Default::default() },
    );
}

#[derive(Default, InherentBuilder, Debug, PartialEq, Eq)]
#[builder(strip_option)]
struct StripOptionStruct {
    a: Option<u32>,
    b: u32,
}

#[test]
fn strip_option_struct() {
    assert_eq!(
        StripOptionStruct::default().a(3).b(6),
        StripOptionStruct { a: Some(3), b: 6 },
    );
    assert_eq!(
        StripOptionStruct::default().b(6),
        StripOptionStruct { a: None, b: 6 },
    );
}

#[derive(Default, InherentBuilder, Debug, PartialEq, Eq)]
#[builder(into, strip_option)]
struct IntoStruct<'a> {
    a: Cow<'a, str>,
    b: Option<Cow<'a, str>>,
}

#[test]
fn into_struct() {
    let testb = "testb".to_string();
    assert_eq!(
        IntoStruct::default().a("testa").b(&testb),
        IntoStruct { a: "testa".into(), b: Some("testb".into()) },
    );
    assert_eq!(
        IntoStruct::default().a("testa").b(testb),
        IntoStruct { a: "testa".into(), b: Some("testb".into()) },
    );
}