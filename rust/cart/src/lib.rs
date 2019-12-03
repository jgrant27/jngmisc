type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Clone, Debug)]
pub struct Sale {
    buy: u64,
    free: u64,
}

impl Sale {
    pub fn new(buy: u64, free: u64) -> Result<Sale> {
        match (buy, free) {
            (0, 0) => Err("quantity to buy and quantity free cannot be zero.")?,
            (0, _) => Err("quantity to buy cannot be zero.")?,
            (_, 0) => Err("quantity free cannot be zero.")?,
            _ => Ok(Sale {
                buy: buy,
                free: free,
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Product {
    name: String,
    price: u64,
    sale: Option<Sale>,
}

impl Product {
    pub fn new(name: &str, price: u64, sale: Option<Sale>) -> Result<Product> {
        match (name, price, sale) {
            (name, _, _) if name.is_empty() => Err("Name cannot be empty")?,
            (_, 0, _) => Err("Price cannot be zero")?,
            (name, price, sale) => Ok(Product {
                name: name.to_string(),
                price: price,
                sale: sale,
            }),
        }
    }
}

pub struct Cart {
    items: Vec<Product>,
}
