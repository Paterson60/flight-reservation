CREATE TABLE `price` (
  `price_id` int AUTO_INCREMENT,
  `amount` DOUBLE NOT NULL,
  PRIMARY KEY (`price_id`)
);

CREATE TABLE `product_association` (
  `association_id` int AUTO_INCREMENT,
  `sku` varchar(255),
  `related_products` varchar(255),
  `bundle_deals` varchar(255),
  `product_variations` varchar(255),
  PRIMARY KEY (`association_id`)
);

CREATE TABLE `product` (
  `product_id` int AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `category` varchar(255),
  `description` varchar(1000),
  `image` varchar(255),
  `specification` varchar(255),
  `sku` varchar(255),
  `price_id` int,
  PRIMARY KEY (`product_id`),
  FOREIGN KEY (`price_id`) REFERENCES `price`(`price_id`) ON DELETE CASCADE
);

