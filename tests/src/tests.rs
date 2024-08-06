use std::{collections::HashMap, ops::RangeInclusive};

use rsn::FromValue;

#[test]
fn test_rsn_macro() {
    let test_integer = &2;
    let value = rsn::rsn! {
        Test {
            a: #test_integer,
            b: "hello!",
        }
    };
    #[derive(FromValue)]
    struct Test {
        a: u32,
        b: String,
    }

    let test: Test = FromValue::<(), &str>::from_value(value, &mut ()).unwrap();

    assert_eq!(test.a, *test_integer);
    assert_eq!(test.b, "hello!");
}

fn de<'a, T: rsn::FromValue<(), &'a str>>(str: &'a str) -> T {
    match rsn::de(str) {
        Ok(t) => t,
        Err(err) => panic!("\n{err}\n"),
    }
}

macro_rules! test_repr {
    ($expr:expr $(; $ty:ty $(; $conv:ident)?)?) => {
        let t $(: $ty)? = $expr $($(.$conv())?)?;
        assert_eq!(t, de $(::<$ty>)? (stringify!($expr)))
    };
}

#[test]
fn test_integer() {
    test_repr!(123);
    test_repr!(-123);
    test_repr!(123; u32);
    test_repr!(0b100101010);
    test_repr!(0o214735);
    test_repr!(0xF12AE);
}

#[test]
fn test_floating() {
    println!("{}", "5.1e8".parse::<f32>().unwrap());
    test_repr!(0.1);
    test_repr!(-2.14);
    test_repr!(5.1e8);
    test_repr!(5e-8);
}

#[test]
fn test_string() {
    test_repr!("Hello💖World!"; String; to_string);
    test_repr!("Hello\nWorld!"; String; to_string);
    test_repr!("\\ \" \\ \t"; String; to_string);
}

#[test]
fn test_char() {
    test_repr!('A');
    test_repr!('💖');
    test_repr!('\n');
    test_repr!('\\');
}

#[test]
fn test_array() {
    test_repr!([0, 1, 2, 3]; [u32; 4]);
    test_repr!([]; [String; 0]);
    test_repr!([[1, 2], [2, 3], [5, 6]]; [[u8; 2]; 3]);
}

#[test]
fn test_map() {
    assert!(de::<HashMap::<i32, i32>>("{}").is_empty());

    let control: HashMap<String, i32> = HashMap::from_iter([
        ("cat".to_string(), 2),
        ("dog".to_string(), 3),
        ("dinosaur".to_string(), 35),
        ("car".to_string(), 100),
        ("human".to_string(), -5),
    ]);
    let bytes = br#"
    {
        "cat" => 2,
        "dog" => 3,
        "dinosaur" => 35,
        "car" => 100,
        "human" => -5,
    }
    "#;
    let parsed: HashMap<String, i32> = de(std::str::from_utf8(bytes).unwrap());

    assert_eq!(control, parsed);
}

#[test]
fn test_range() {
    test_repr!(..);
    test_repr!(12..);
    test_repr!(..23);
    test_repr!(..=33);
    test_repr!(12..23);
    test_repr!(12..=23);
    test_repr!((2, 'c')..=(123, 'z'));
}

#[test]
fn test_tuple() {
    test_repr!(());
    test_repr!((1,));
    test_repr!((1..=2, 3, 'Z'));
}

#[test]
fn test_struct_derive() {
    #[derive(Debug, PartialEq, FromValue)]
    struct Test<T>(T);

    #[derive(Debug, PartialEq, FromValue)]
    struct Test2<T> {
        a: T,
        b: T,
    }

    #[derive(Debug, PartialEq, FromValue)]
    struct Foo;

    #[derive(Debug, PartialEq, FromValue)]
    struct Bar(i32);

    #[derive(Debug, PartialEq, FromValue)]
    struct Baz(i32, u32, (Foo, Bar));

    #[derive(Debug, PartialEq, FromValue)]
    struct Struct {
        baz: Baz,
        array: [Bar; 3],
        tuple: (Foo, u32, i32),
        range: RangeInclusive<f32>,
    }

    test_repr!(Test(2));
    test_repr!(Test2 { a: 3, b: 4 });

    test_repr!(Foo);

    test_repr!(Bar(2));

    test_repr!(Baz(-5, 10, (Foo, Bar(3))));

    test_repr!(Struct {
        baz: Baz(-45, 20, (Foo, Bar(100))),
        array: [Bar(10), Bar(-2), Bar(0)],
        tuple: (Foo, 10, -10),
        range: 0.5..=10.0,
    });
}

#[test]
fn test_enum_derive() {
    #[derive(Debug, PartialEq, FromValue)]
    enum Enum {
        A,
        B,
        C,
        D,
        E,
    }

    #[derive(Debug, PartialEq, FromValue)]
    enum Foo {
        A(u32),
        B(i32),
        C(bool),
    }

    test_repr!(Enum::A);
    test_repr!(Enum::D);

    test_repr!(Foo::A(32));
    test_repr!(Foo::B(-21));
    test_repr!(Foo::C(true));

    #[derive(Debug, PartialEq, FromValue)]
    #[rsn(untagged)]
    enum UntaggedEnum {
        A,
        B,
        C,
        D,
        E,
    }

    let parsed: UntaggedEnum = de("C");
    assert_eq!(UntaggedEnum::C, parsed);

    #[derive(Debug, PartialEq, FromValue)]
    struct IRecurse(Box<Recursive>);

    #[derive(Debug, PartialEq, FromValue)]
    #[rsn(untagged)]
    enum Recursive {
        #[rsn(untagged)]
        Node(Box<Recursive>, Box<Recursive>),
        #[rsn(untagged)]
        Leaf(UntaggedEnum),
        Test(#[rsn(skip_bound)] IRecurse),
    }

    let parsed: Recursive = de("
        (
            (A, (E, B)),
            (
                (
                    (C, A),
                    ((B, A), D),
                ),
                E,
            ),
        )
    ");

    assert_eq!(
        parsed,
        Recursive::Node(
            Box::new(Recursive::Node(
                Box::new(Recursive::Leaf(UntaggedEnum::A)),
                Box::new(Recursive::Node(
                    Box::new(Recursive::Leaf(UntaggedEnum::E)),
                    Box::new(Recursive::Leaf(UntaggedEnum::B))
                ))
            )),
            Box::new(Recursive::Node(
                Box::new(Recursive::Node(
                    Box::new(Recursive::Node(
                        Box::new(Recursive::Leaf(UntaggedEnum::C)),
                        Box::new(Recursive::Leaf(UntaggedEnum::A))
                    )),
                    Box::new(Recursive::Node(
                        Box::new(Recursive::Node(
                            Box::new(Recursive::Leaf(UntaggedEnum::B)),
                            Box::new(Recursive::Leaf(UntaggedEnum::A))
                        )),
                        Box::new(Recursive::Leaf(UntaggedEnum::D)),
                    ))
                )),
                Box::new(Recursive::Leaf(UntaggedEnum::E)),
            ))
        )
    )
}

#[test]
fn test_flatten() {
    #[derive(Debug, PartialEq, FromValue)]
    struct StructA {
        a: u32,
        b: u32,
        c: u32,
    }

    #[derive(Default, Debug, PartialEq, FromValue)]
    struct StructB {
        d: u32,
        e: u32,
        f: u32,
    }

    #[derive(Debug, PartialEq, FromValue)]
    struct Struct {
        #[rsn(flatten)]
        a: StructA,
        #[rsn(default)]
        b: StructB,
    }

    let t: Struct = de("
        Struct {
            a: 1,
            b: 2,
            c: 3,
        }
    ");

    assert_eq!(
        t,
        Struct {
            a: StructA { a: 1, b: 2, c: 3 },
            b: StructB::default(),
        }
    );
}
