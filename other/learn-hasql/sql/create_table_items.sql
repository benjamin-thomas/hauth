CREATE TEMP TABLE items(
    id UUID NOT NULL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    alt_name VARCHAR(100) NULL,
    is_physical BOOLEAN NOT NULL,
    quantity INT NOT NULL CHECK (quantity >= 0)
);