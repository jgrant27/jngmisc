extern crate cart;

use cart::*;

#[test]
fn sale_new_buy_and_free_cannot_be_zero() {
    match Sale::new(0, 0) {
        Ok(_) => assert!(false),
        Err(msg) => assert_eq!(
            msg.to_string(),
            "quantity to buy and quantity free cannot be zero."
        ),
    }
}

#[test]
fn sale_new_buy_cannot_be_zero() {
    match Sale::new(0, 1) {
        Ok(_) => assert!(false),
        Err(msg) => assert_eq!(msg.to_string(), "quantity to buy cannot be zero."),
    }
}

#[test]
fn sale_new_free_cannot_be_zero() {
    match Sale::new(1, 0) {
        Ok(_) => assert!(false),
        Err(msg) => assert_eq!(msg.to_string(), "quantity free cannot be zero."),
    }
}

#[test]
fn sale_new_valid() {
    match Sale::new(1, 1) {
        Ok(_) => assert!(true),
        Err(_) => assert!(false),
    }
}

#[test]
fn product_new_price_cannot_be_zero() {
    match Product::new("", 1, None) {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
    }
}

#[test]
fn product_new_name_cannot_be_empty() {
    match Product::new("", 1, None) {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
    }
}

#[test]
fn product_new_valid() {
    match Product::new("Test Product", 1, None) {
        Ok(_) => assert!(true),
        Err(_) => assert!(false),
    }
}
