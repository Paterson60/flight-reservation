package com.service.productcatalogue.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.time.LocalDate;
import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

class ProductTest {

    @Test
    void testConstructor() {
        Product actualProduct = new Product();
        actualProduct.setBrand("Brand");
        actualProduct.setCategory("Category");
        LocalDateTime createdAt = LocalDate.of(1970, 1, 1).atStartOfDay();
        actualProduct.setCreatedAt(createdAt);
        actualProduct.setDescription("The characteristics of someone or something");
        actualProduct.setImage("Image");
        actualProduct.setName("Name");
        Price price = new Price();
        price.setAmount(10L);
        price.setDiscount(3L);
        price.setPriceId(1L);
        actualProduct.setPrice(price);
        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(1L);
        productAssociation.setBundleDeals("Bundle Deals");
        productAssociation.setProductVariations("Product Variations");
        productAssociation.setRelatedProducts("Related Products");
        productAssociation.setSku("Sku");
        actualProduct.setProductAssociation(productAssociation);
        actualProduct.setProductId(1L);
        actualProduct.setSku("Sku");
        actualProduct.setSpecification("Specification");
        LocalDateTime updatedAt = LocalDate.of(1970, 1, 1).atStartOfDay();
        actualProduct.setUpdatedAt(updatedAt);
        String actualToStringResult = actualProduct.toString();
        String actualBrand = actualProduct.getBrand();
        String actualCategory = actualProduct.getCategory();
        LocalDateTime actualCreatedAt = actualProduct.getCreatedAt();
        String actualDescription = actualProduct.getDescription();
        String actualImage = actualProduct.getImage();
        String actualName = actualProduct.getName();
        Price actualPrice = actualProduct.getPrice();
        ProductAssociation actualProductAssociation = actualProduct.getProductAssociation();
        Long actualProductId = actualProduct.getProductId();
        String actualSku = actualProduct.getSku();
        String actualSpecification = actualProduct.getSpecification();
        LocalDateTime actualUpdatedAt = actualProduct.getUpdatedAt();
        assertEquals("Brand", actualBrand);
        assertEquals("Category", actualCategory);
        assertEquals("Image", actualImage);
        assertEquals("Name", actualName);
        assertEquals("Product(productId=1, name=Name, category=Category, description=The characteristics of someone or"
                + " something, image=Image, specification=Specification, sku=Sku, brand=Brand, createdAt=1970-01-01T00:00,"
                + " updatedAt=1970-01-01T00:00, price=Price(priceId=1, amount=10, discount=3), productAssociation"
                + "=ProductAssociation(associationId=1, sku=Sku, relatedProducts=Related Products, bundleDeals=Bundle"
                + " Deals, productVariations=Product Variations))", actualToStringResult);
        assertEquals("Sku", actualSku);
        assertEquals("Specification", actualSpecification);
        assertEquals("The characteristics of someone or something", actualDescription);
        assertEquals(1L, actualProductId.longValue());
        assertSame(price, actualPrice);
        assertSame(productAssociation, actualProductAssociation);
        assertSame(createdAt, actualCreatedAt);
        assertSame(updatedAt, actualUpdatedAt);
    }


    @Test
    void testConstructor2() {
        LocalDateTime createdAt = LocalDate.of(1970, 1, 1).atStartOfDay();
        LocalDateTime updatedAt = LocalDate.of(1970, 1, 1).atStartOfDay();

        Price price = new Price();
        price.setAmount(10L);
        price.setDiscount(3L);
        price.setPriceId(1L);

        ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(1L);
        productAssociation.setBundleDeals("Bundle Deals");
        productAssociation.setProductVariations("Product Variations");
        productAssociation.setRelatedProducts("Related Products");
        productAssociation.setSku("Sku");
        Product actualProduct = new Product(1L, "Name", "Category", "The characteristics of someone or something", "Image",
                "Specification", "Sku", "Brand", createdAt, updatedAt, price, productAssociation);
        actualProduct.setBrand("Brand");
        actualProduct.setCategory("Category");
        LocalDateTime createdAt2 = LocalDate.of(1970, 1, 1).atStartOfDay();
        actualProduct.setCreatedAt(createdAt2);
        actualProduct.setDescription("The characteristics of someone or something");
        actualProduct.setImage("Image");
        actualProduct.setName("Name");
        Price price2 = new Price();
        price2.setAmount(10L);
        price2.setDiscount(3L);
        price2.setPriceId(1L);
        actualProduct.setPrice(price2);
        ProductAssociation productAssociation2 = new ProductAssociation();
        productAssociation2.setAssociationId(1L);
        productAssociation2.setBundleDeals("Bundle Deals");
        productAssociation2.setProductVariations("Product Variations");
        productAssociation2.setRelatedProducts("Related Products");
        productAssociation2.setSku("Sku");
        actualProduct.setProductAssociation(productAssociation2);
        actualProduct.setProductId(1L);
        actualProduct.setSku("Sku");
        actualProduct.setSpecification("Specification");
        LocalDateTime updatedAt2 = LocalDate.of(1970, 1, 1).atStartOfDay();
        actualProduct.setUpdatedAt(updatedAt2);
        String actualToStringResult = actualProduct.toString();
        String actualBrand = actualProduct.getBrand();
        String actualCategory = actualProduct.getCategory();
        LocalDateTime actualCreatedAt = actualProduct.getCreatedAt();
        String actualDescription = actualProduct.getDescription();
        String actualImage = actualProduct.getImage();
        String actualName = actualProduct.getName();
        Price actualPrice = actualProduct.getPrice();
        ProductAssociation actualProductAssociation = actualProduct.getProductAssociation();
        Long actualProductId = actualProduct.getProductId();
        String actualSku = actualProduct.getSku();
        String actualSpecification = actualProduct.getSpecification();
        LocalDateTime actualUpdatedAt = actualProduct.getUpdatedAt();
        assertEquals("Brand", actualBrand);
        assertEquals("Category", actualCategory);
        assertEquals("Image", actualImage);
        assertEquals("Name", actualName);
        assertEquals("Product(productId=1, name=Name, category=Category, description=The characteristics of someone or"
                + " something, image=Image, specification=Specification, sku=Sku, brand=Brand, createdAt=1970-01-01T00:00,"
                + " updatedAt=1970-01-01T00:00, price=Price(priceId=1, amount=10, discount=3), productAssociation"
                + "=ProductAssociation(associationId=1, sku=Sku, relatedProducts=Related Products, bundleDeals=Bundle"
                + " Deals, productVariations=Product Variations))", actualToStringResult);
        assertEquals("Sku", actualSku);
        assertEquals("Specification", actualSpecification);
        assertEquals("The characteristics of someone or something", actualDescription);
        assertEquals(1L, actualProductId.longValue());
        assertSame(price2, actualPrice);
        assertSame(productAssociation2, actualProductAssociation);
        assertSame(createdAt2, actualCreatedAt);
        assertSame(updatedAt2, actualUpdatedAt);
    }
}
