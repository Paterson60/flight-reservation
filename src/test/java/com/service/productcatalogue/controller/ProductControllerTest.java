package com.service.productcatalogue.controller;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.service.productcatalogue.dto.AddAllProductDetailsDto;
import com.service.productcatalogue.dto.PriceDto;
import com.service.productcatalogue.dto.ProductAssociationDto;
import com.service.productcatalogue.dto.ProductDto;
import com.service.productcatalogue.dto.ProductSearchCriteriaDto;
import com.service.productcatalogue.service.IProductService;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@ContextConfiguration(classes = {ProductController.class})
@ExtendWith(SpringExtension.class)
class ProductControllerTest {
    @MockBean
    private IProductService iProductService;

    @Autowired
    private ProductController productController;

    /**
     * Method under test:
     * {@link ProductController#addProduct(AddAllProductDetailsDto)}
     */
    @Test
    void testAddProduct() throws Exception {
        doNothing().when(iProductService).addProduct(Mockito.<AddAllProductDetailsDto>any());

        AddAllProductDetailsDto addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setAmount(10L);
        addAllProductDetailsDto.setBrand("Brand");
        addAllProductDetailsDto.setBundleDeals("Bundle Deals");
        addAllProductDetailsDto.setCategory("Category");
        addAllProductDetailsDto.setDescription("The characteristics of someone or something");
        addAllProductDetailsDto.setDiscount(3L);
        addAllProductDetailsDto.setImage("Image");
        addAllProductDetailsDto.setName("Name");
        addAllProductDetailsDto.setProductVariations("Product Variations");
        addAllProductDetailsDto.setRelatedProducts("Related Products");
        addAllProductDetailsDto.setSku("Sku");
        addAllProductDetailsDto.setSpecification("Specification");
        String content = (new ObjectMapper()).writeValueAsString(addAllProductDetailsDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.post("/api/addProduct")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        ResultActions actualPerformResult = MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder);
        actualPerformResult.andExpect(MockMvcResultMatchers.status().isCreated())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string("{\"statusCode\":\"201\",\"statusMsg\":\"Product added successfully\"}"));
    }

    /**
     * Method under test: {@link ProductController#fetchAllProducts()}
     */
    @Test
    void testFetchAllProducts() throws Exception {
        when(iProductService.fetchAllProducts()).thenReturn(new ArrayList<>());
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.get("/api/fetchAllProducts");
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content().string("[]"));
    }

    /**
     * Method under test: {@link ProductController#updateProduct(ProductDto)}
     */
    @Test
    void testUpdateProduct() throws Exception {
        when(iProductService.updateProduct(Mockito.<ProductDto>any())).thenReturn(true);

        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(10L);
        priceDto.setDiscount(3L);

        ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setBundleDeals("Bundle Deals");
        productAssociationDto.setProductVariations("Product Variations");
        productAssociationDto.setRelatedProducts("Related Products");
        productAssociationDto.setSku("Sku");

        ProductDto productDto = new ProductDto();
        productDto.setBrand("Brand");
        productDto.setCategory("Category");
        productDto.setDescription("The characteristics of someone or something");
        productDto.setImage("Image");
        productDto.setName("Name");
        productDto.setPriceDto(priceDto);
        productDto.setProductAssociationDto(productAssociationDto);
        productDto.setSku("Sku");
        productDto.setSpecification("Specification");
        String content = (new ObjectMapper()).writeValueAsString(productDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.put("/api/updateProduct")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string("{\"statusCode\":\"200\",\"statusMsg\":\"Request processed successfully\"}"));
    }

    /**
     * Method under test: {@link ProductController#updateProduct(ProductDto)}
     */
    @Test
    void testUpdateProduct2() throws Exception {
        when(iProductService.updateProduct(Mockito.<ProductDto>any())).thenReturn(false);

        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(10L);
        priceDto.setDiscount(3L);

        ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setBundleDeals("Bundle Deals");
        productAssociationDto.setProductVariations("Product Variations");
        productAssociationDto.setRelatedProducts("Related Products");
        productAssociationDto.setSku("Sku");

        ProductDto productDto = new ProductDto();
        productDto.setBrand("Brand");
        productDto.setCategory("Category");
        productDto.setDescription("The characteristics of someone or something");
        productDto.setImage("Image");
        productDto.setName("Name");
        productDto.setPriceDto(priceDto);
        productDto.setProductAssociationDto(productAssociationDto);
        productDto.setSku("Sku");
        productDto.setSpecification("Specification");
        String content = (new ObjectMapper()).writeValueAsString(productDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.put("/api/updateProduct")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        ResultActions actualPerformResult = MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder);
        actualPerformResult.andExpect(MockMvcResultMatchers.status().is(417))
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string(
                                "{\"statusCode\":\"417\",\"statusMsg\":\"Update operation failed. Please try again or contact help team\"}"));
    }

    /**
     * Method under test: {@link ProductController#deleteProduct(String)}
     */
    @Test
    void testDeleteProduct() throws Exception {
        when(iProductService.deleteProduct(Mockito.<String>any())).thenReturn(true);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.delete("/api/deleteProduct")
                .param("sku", "foo");
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string("{\"statusCode\":\"200\",\"statusMsg\":\"Request processed successfully\"}"));
    }

    /**
     * Method under test: {@link ProductController#deleteProduct(String)}
     */
    @Test
    void testDeleteProduct2() throws Exception {
        when(iProductService.deleteProduct(Mockito.<String>any())).thenReturn(false);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.delete("/api/deleteProduct")
                .param("sku", "foo");
        ResultActions actualPerformResult = MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder);
        actualPerformResult.andExpect(MockMvcResultMatchers.status().is(417))
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string(
                                "{\"statusCode\":\"417\",\"statusMsg\":\"Delete operation failed. Please try again or contact help team\"}"));
    }

    /**
     * Method under test:
     * {@link ProductController#searchProducts(ProductSearchCriteriaDto)}
     */
    @Test
    void testSearchProducts() throws Exception {
        when(iProductService.searchProducts(Mockito.<ProductSearchCriteriaDto>any())).thenReturn(new ArrayList<>());

        ProductSearchCriteriaDto productSearchCriteriaDto = new ProductSearchCriteriaDto();
        productSearchCriteriaDto.setAmount(10L);
        productSearchCriteriaDto.setBrand("Brand");
        productSearchCriteriaDto.setCategory("Category");
        productSearchCriteriaDto.setDescription("The characteristics of someone or something");
        productSearchCriteriaDto.setImage("Image");
        productSearchCriteriaDto.setName("Name");
        productSearchCriteriaDto.setSku("Sku");
        productSearchCriteriaDto.setSpecification("Specification");
        String content = (new ObjectMapper()).writeValueAsString(productSearchCriteriaDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.post("/api/searchProducts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        ResultActions actualPerformResult = MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder);
        actualPerformResult.andExpect(MockMvcResultMatchers.status().is(417))
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string(
                                "{\"statusCode\":\"500\",\"statusMsg\":\"Product not found. Please try again or contact help team\"}"));
    }

    /**
     * Method under test:
     * {@link ProductController#searchProducts(ProductSearchCriteriaDto)}
     */
    @Test
    void testSearchProducts2() throws Exception {
        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(10L);
        priceDto.setDiscount(3L);

        ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setBundleDeals("500");
        productAssociationDto.setProductVariations("500");
        productAssociationDto.setRelatedProducts("500");
        productAssociationDto.setSku("500");

        ProductDto productDto = new ProductDto();
        productDto.setBrand("500");
        productDto.setCategory("500");
        productDto.setDescription("The characteristics of someone or something");
        productDto.setImage("500");
        productDto.setName("500");
        productDto.setPriceDto(priceDto);
        productDto.setProductAssociationDto(productAssociationDto);
        productDto.setSku("500");
        productDto.setSpecification("500");

        ArrayList<ProductDto> productDtoList = new ArrayList<>();
        productDtoList.add(productDto);
        when(iProductService.searchProducts(Mockito.<ProductSearchCriteriaDto>any())).thenReturn(productDtoList);

        ProductSearchCriteriaDto productSearchCriteriaDto = new ProductSearchCriteriaDto();
        productSearchCriteriaDto.setAmount(10L);
        productSearchCriteriaDto.setBrand("Brand");
        productSearchCriteriaDto.setCategory("Category");
        productSearchCriteriaDto.setDescription("The characteristics of someone or something");
        productSearchCriteriaDto.setImage("Image");
        productSearchCriteriaDto.setName("Name");
        productSearchCriteriaDto.setSku("Sku");
        productSearchCriteriaDto.setSpecification("Specification");
        String content = (new ObjectMapper()).writeValueAsString(productSearchCriteriaDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.post("/api/searchProducts")
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string(
                                "[{\"name\":\"500\",\"category\":\"500\",\"description\":\"The characteristics of someone or something\",\"image"
                                        + "\":\"500\",\"specification\":\"500\",\"sku\":\"500\",\"brand\":\"500\",\"productAssociationDto\":{\"sku\":\"500\","
                                        + "\"relatedProducts\":\"500\",\"bundleDeals\":\"500\",\"productVariations\":\"500\"},\"priceDto\":{\"amount\":10,"
                                        + "\"discount\":3}}]"));
    }

    /**
     * Method under test: {@link ProductController#updatePrice(Long, PriceDto)}
     */
    @Test
    void testUpdatePrice() throws Exception {
        doNothing().when(iProductService).updatePrice(Mockito.<Long>any(), Mockito.<PriceDto>any());

        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(10L);
        priceDto.setDiscount(3L);
        String content = (new ObjectMapper()).writeValueAsString(priceDto);
        MockHttpServletRequestBuilder putResult = MockMvcRequestBuilders.put("/api/updatePrice");
        MockHttpServletRequestBuilder requestBuilder = putResult.param("productId", String.valueOf(1L))
                .contentType(MediaType.APPLICATION_JSON)
                .content(content);
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string("{\"statusCode\":\"200\",\"statusMsg\":\"Request processed successfully\"}"));
    }

    /**
     * Method under test: {@link ProductController#fetchProductDetails(String)}
     */
    @Test
    void testFetchProductDetails() throws Exception {
        PriceDto priceDto = new PriceDto();
        priceDto.setAmount(10L);
        priceDto.setDiscount(3L);

        ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setBundleDeals("Bundle Deals");
        productAssociationDto.setProductVariations("Product Variations");
        productAssociationDto.setRelatedProducts("Related Products");
        productAssociationDto.setSku("Sku");

        ProductDto productDto = new ProductDto();
        productDto.setBrand("Brand");
        productDto.setCategory("Category");
        productDto.setDescription("The characteristics of someone or something");
        productDto.setImage("Image");
        productDto.setName("Name");
        productDto.setPriceDto(priceDto);
        productDto.setProductAssociationDto(productAssociationDto);
        productDto.setSku("Sku");
        productDto.setSpecification("Specification");
        when(iProductService.fetchProduct(Mockito.<String>any())).thenReturn(productDto);
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders.get("/api/fetchProduct").param("sku", "foo");
        MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder)
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content()
                        .string(
                                "{\"name\":\"Name\",\"category\":\"Category\",\"description\":\"The characteristics of someone or something\",\"image"
                                        + "\":\"Image\",\"specification\":\"Specification\",\"sku\":\"Sku\",\"brand\":\"Brand\",\"productAssociationDto\":{\"sku\""
                                        + ":\"Sku\",\"relatedProducts\":\"Related Products\",\"bundleDeals\":\"Bundle Deals\",\"productVariations\":\"Product"
                                        + " Variations\"},\"priceDto\":{\"amount\":10,\"discount\":3}}"));
    }

    /**
     * Method under test:
     * {@link ProductController#productPagination(Integer, Integer, String)}
     */
    @Test
    void testProductPagination() throws Exception {
        when(iProductService.getProductPagination(Mockito.<Integer>any(), Mockito.<Integer>any(), Mockito.<String>any()))
                .thenReturn(new PageImpl<>(new ArrayList<>()));
        MockHttpServletRequestBuilder requestBuilder = MockMvcRequestBuilders
                .get("/api/paginAndSortingProducts/{pageNumber}/{pageSize}/{sortProperty}", 10, 3, "Sort Property");
        ResultActions actualPerformResult = MockMvcBuilders.standaloneSetup(productController)
                .build()
                .perform(requestBuilder);
        actualPerformResult.andExpect(MockMvcResultMatchers.status().is(500))
                .andExpect(MockMvcResultMatchers.content().contentType("application/json"))
                .andExpect(MockMvcResultMatchers.content().string(""));
    }
}
