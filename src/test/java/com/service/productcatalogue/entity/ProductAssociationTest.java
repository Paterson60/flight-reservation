package com.service.productcatalogue.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class ProductAssociationTest {

    @Test
    void testConstructor() {
        ProductAssociation actualProductAssociation = new ProductAssociation();
        actualProductAssociation.setAssociationId(1L);
        actualProductAssociation.setBundleDeals("Bundle Deals");
        actualProductAssociation.setProductVariations("Product Variations");
        actualProductAssociation.setRelatedProducts("Related Products");
        actualProductAssociation.setSku("Sku");
        String actualToStringResult = actualProductAssociation.toString();
        Long actualAssociationId = actualProductAssociation.getAssociationId();
        String actualBundleDeals = actualProductAssociation.getBundleDeals();
        String actualProductVariations = actualProductAssociation.getProductVariations();
        String actualRelatedProducts = actualProductAssociation.getRelatedProducts();
        assertEquals("Bundle Deals", actualBundleDeals);
        assertEquals("Product Variations", actualProductVariations);
        assertEquals("ProductAssociation(associationId=1, sku=Sku, relatedProducts=Related Products, bundleDeals=Bundle"
                + " Deals, productVariations=Product Variations)", actualToStringResult);
        assertEquals("Related Products", actualRelatedProducts);
        assertEquals("Sku", actualProductAssociation.getSku());
        assertEquals(1L, actualAssociationId.longValue());
    }


    @Test
    void testConstructor2() {
        ProductAssociation actualProductAssociation = new ProductAssociation(1L, "Sku", "Related Products", "Bundle Deals",
                "Product Variations");
        actualProductAssociation.setAssociationId(1L);
        actualProductAssociation.setBundleDeals("Bundle Deals");
        actualProductAssociation.setProductVariations("Product Variations");
        actualProductAssociation.setRelatedProducts("Related Products");
        actualProductAssociation.setSku("Sku");
        String actualToStringResult = actualProductAssociation.toString();
        Long actualAssociationId = actualProductAssociation.getAssociationId();
        String actualBundleDeals = actualProductAssociation.getBundleDeals();
        String actualProductVariations = actualProductAssociation.getProductVariations();
        String actualRelatedProducts = actualProductAssociation.getRelatedProducts();
        assertEquals("Bundle Deals", actualBundleDeals);
        assertEquals("Product Variations", actualProductVariations);
        assertEquals("ProductAssociation(associationId=1, sku=Sku, relatedProducts=Related Products, bundleDeals=Bundle"
                + " Deals, productVariations=Product Variations)", actualToStringResult);
        assertEquals("Related Products", actualRelatedProducts);
        assertEquals("Sku", actualProductAssociation.getSku());
        assertEquals(1L, actualAssociationId.longValue());
    }
}
