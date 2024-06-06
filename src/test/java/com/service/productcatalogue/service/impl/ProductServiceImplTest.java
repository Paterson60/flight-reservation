package com.service.productcatalogue.service.impl;

import com.service.productcatalogue.dto.*;
import com.service.productcatalogue.entity.Price;
import com.service.productcatalogue.entity.Product;
import com.service.productcatalogue.entity.ProductAssociation;
import com.service.productcatalogue.exception.ProductExistsException;
import com.service.productcatalogue.exception.ResourceNotFoundException;
import com.service.productcatalogue.repository.PriceRepository;
import com.service.productcatalogue.repository.ProductAssociationRepository;
import com.service.productcatalogue.repository.ProductRepository;
import org.assertj.core.api.AbstractStringAssert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.*;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ProductServiceImplTest {

    @Mock
    private ProductRepository mockProductRepository;
    @Mock
    private PriceRepository mockPriceRepository;
    @Mock
    private ProductAssociationRepository mockProductAssociationRepository;

    private ProductServiceImpl productServiceImplUnderTest;

    @BeforeEach
    void setUp() {
        productServiceImplUnderTest = new ProductServiceImpl(mockProductRepository, mockPriceRepository,
                mockProductAssociationRepository);
    }

    @Test
    public void test_returns_matching_products_when_all_criteria_match() {

        final ProductSearchCriteriaDto searchCriteria = new ProductSearchCriteriaDto();
        searchCriteria.setName("name");
        searchCriteria.setCategory("category");
        searchCriteria.setDescription("description");
        searchCriteria.setImage("image");
        searchCriteria.setSpecification("specification");
        searchCriteria.setSku("sku");
        searchCriteria.setBrand("brand");
        searchCriteria.setAmount(1000L);

        Product product = new Product();
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        Price price = new Price();
        price.setAmount(1000L);
        product.setPrice(price);

        List<Product> products = Collections.singletonList(product);

        when(mockProductRepository.findAll()).thenReturn(products);


        final List<ProductDto> result = productServiceImplUnderTest.searchProducts(searchCriteria);


        assertThat(result).hasSize(0);
    }

    @Test
    public void test_handles_null_search_criteria_gracefully() {

        when(mockProductRepository.findAll()).thenReturn(Collections.emptyList());

        final List<ProductDto> result = productServiceImplUnderTest.searchProducts(null);

        assertThat(result).isEqualTo(Collections.emptyList());
    }

    @Test
    public void test_returns_page_of_products_sorted_by_specified_property() {
        ProductRepository productRepository = mock(ProductRepository.class);
        ProductServiceImpl productService = new ProductServiceImpl(productRepository, null, null);

        int pageNumber = 0;
        int pageSize = 5;
        String sortProperty = "name";

        Pageable pageable = PageRequest.of(pageNumber, pageSize, Sort.Direction.ASC, sortProperty);
        Page<Product> expectedPage = mock(Page.class);

        when(productRepository.findAll(pageable)).thenReturn(expectedPage);

        Page<Product> resultPage = productService.getProductPagination(pageNumber, pageSize, sortProperty);

        assertEquals(expectedPage, resultPage);
        verify(productRepository).findAll(pageable);
    }

    @Test
    void testAddProduct_ThrowsProductExistsException() {

        final AddAllProductDetailsDto addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setName("name");
        addAllProductDetailsDto.setCategory("category");
        addAllProductDetailsDto.setDescription("description");
        addAllProductDetailsDto.setImage("image");
        addAllProductDetailsDto.setSpecification("specification");
        addAllProductDetailsDto.setSku("sku");
        addAllProductDetailsDto.setBrand("brand");
        addAllProductDetailsDto.setRelatedProducts("relatedProducts");
        addAllProductDetailsDto.setBundleDeals("bundleDeals");
        addAllProductDetailsDto.setProductVariations("productVariations");
        addAllProductDetailsDto.setAmount(0L);
        addAllProductDetailsDto.setDiscount(0L);

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        assertThatThrownBy(() -> productServiceImplUnderTest.addProduct(addAllProductDetailsDto))
                .isInstanceOf(ProductExistsException.class);
        verify(mockProductAssociationRepository).save(any(ProductAssociation.class));
        verify(mockPriceRepository).save(any(Price.class));
    }

    @Test
    void testAddProduct_ProductRepositoryFindBySkuReturnsAbsent() {
        // Setup
        final AddAllProductDetailsDto addAllProductDetailsDto = new AddAllProductDetailsDto();
        addAllProductDetailsDto.setName("name");
        addAllProductDetailsDto.setCategory("category");
        addAllProductDetailsDto.setDescription("description");
        addAllProductDetailsDto.setImage("image");
        addAllProductDetailsDto.setSpecification("specification");
        addAllProductDetailsDto.setSku("sku");
        addAllProductDetailsDto.setBrand("brand");
        addAllProductDetailsDto.setRelatedProducts("relatedProducts");
        addAllProductDetailsDto.setBundleDeals("bundleDeals");
        addAllProductDetailsDto.setProductVariations("productVariations");
        addAllProductDetailsDto.setAmount(0L);
        addAllProductDetailsDto.setDiscount(0L);

        when(mockProductRepository.findBySku("sku")).thenReturn(Optional.empty());

        productServiceImplUnderTest.addProduct(addAllProductDetailsDto);

        verify(mockProductAssociationRepository).save(any(ProductAssociation.class));
        verify(mockPriceRepository).save(any(Price.class));
        verify(mockProductRepository).save(any(Product.class));
    }

    @Test
    void testFetchProduct() {

        final ProductDto expectedResult = new ProductDto();
        expectedResult.setName("name");
        expectedResult.setCategory("category");
        expectedResult.setDescription("description");
        expectedResult.setImage("image");
        expectedResult.setSpecification("specification");
        expectedResult.setSku("sku");
        expectedResult.setBrand("brand");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");
        expectedResult.setProductAssociationDto(productAssociationDto);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);
        expectedResult.setPriceDto(priceDto);

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        final Optional<ProductAssociation> productAssociation1 = Optional.of(
                new ProductAssociation(0L, "sku", "relatedProducts", "bundleDeals", "productVariations"));
        when(mockProductAssociationRepository.findBySku("sku")).thenReturn(productAssociation1);

        when(mockPriceRepository.findByPriceId(0L)).thenReturn(new Price(0L, 0L, 0L));

        final ProductDto result = productServiceImplUnderTest.fetchProduct("sku");

        assertThat(result).isEqualTo(expectedResult);
    }

    @Test
    void testFetchProduct_ProductRepositoryReturnsAbsent() {

        when(mockProductRepository.findBySku("sku")).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.fetchProduct("sku"))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testFetchProduct_ProductAssociationRepositoryReturnsAbsent() {

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        when(mockProductAssociationRepository.findBySku("sku")).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.fetchProduct("sku"))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testFetchAllProducts() {

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final List<Product> products = List.of(product);
        when(mockProductRepository.findAll()).thenReturn(products);

        final List<Product> result = productServiceImplUnderTest.fetchAllProducts();

    }

    @Test
    void testFetchAllProducts_ProductRepositoryReturnsNoItems() {

        when(mockProductRepository.findAll()).thenReturn(Collections.emptyList());

        final List<Product> result = productServiceImplUnderTest.fetchAllProducts();

        assertThat(result).isEqualTo(Collections.emptyList());
    }

    @Test
    void testUpdateProduct() {

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");
        productDto.setProductAssociationDto(productAssociationDto);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);
        productDto.setPriceDto(priceDto);

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        final Optional<ProductAssociation> productAssociation1 = Optional.of(
                new ProductAssociation(0L, "sku", "relatedProducts", "bundleDeals", "productVariations"));
        when(mockProductAssociationRepository.findBySku("sku")).thenReturn(productAssociation1);

        final ProductAssociation productAssociation2 = new ProductAssociation(0L, "sku", "relatedProducts",
                "bundleDeals", "productVariations");
        when(mockProductAssociationRepository.save(any(ProductAssociation.class))).thenReturn(productAssociation2);

        final Product product1 = new Product();
        product1.setProductId(0L);
        product1.setName("name");
        product1.setCategory("category");
        product1.setDescription("description");
        product1.setImage("image");
        product1.setSpecification("specification");
        product1.setSku("sku");
        product1.setBrand("brand");
        final Price price1 = new Price();
        price1.setAmount(0L);
        price1.setDiscount(0L);
        product1.setPrice(price1);
        final ProductAssociation productAssociation3 = new ProductAssociation();
        productAssociation3.setAssociationId(0L);
        productAssociation3.setSku("sku");
        productAssociation3.setRelatedProducts("relatedProducts");
        productAssociation3.setBundleDeals("bundleDeals");
        productAssociation3.setProductVariations("productVariations");
        product1.setProductAssociation(productAssociation3);
        final Optional<Product> optional1 = Optional.of(product1);
        when(mockProductRepository.findById(0L)).thenReturn(optional1);

        final boolean result = productServiceImplUnderTest.updateProduct(productDto);

        assertThat(result).isTrue();
        verify(mockProductRepository).save(any(Product.class));
    }

    @Test
    void testUpdateProduct_ProductRepositoryFindBySkuReturnsAbsent() {

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");
        productDto.setProductAssociationDto(productAssociationDto);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);
        productDto.setPriceDto(priceDto);

        when(mockProductRepository.findBySku("sku")).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.updateProduct(productDto))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testUpdateProduct_ProductAssociationRepositoryFindBySkuReturnsAbsent() {

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");
        productDto.setProductAssociationDto(productAssociationDto);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);
        productDto.setPriceDto(priceDto);

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        when(mockProductAssociationRepository.findBySku("sku")).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.updateProduct(productDto))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testUpdateProduct_ProductRepositoryFindByIdReturnsAbsent() {

        final ProductDto productDto = new ProductDto();
        productDto.setName("name");
        productDto.setCategory("category");
        productDto.setDescription("description");
        productDto.setImage("image");
        productDto.setSpecification("specification");
        productDto.setSku("sku");
        productDto.setBrand("brand");
        final ProductAssociationDto productAssociationDto = new ProductAssociationDto();
        productAssociationDto.setSku("sku");
        productAssociationDto.setRelatedProducts("relatedProducts");
        productAssociationDto.setBundleDeals("bundleDeals");
        productAssociationDto.setProductVariations("productVariations");
        productDto.setProductAssociationDto(productAssociationDto);
        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);
        productDto.setPriceDto(priceDto);

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        final Optional<ProductAssociation> productAssociation1 = Optional.of(
                new ProductAssociation(0L, "sku", "relatedProducts", "bundleDeals", "productVariations"));
        when(mockProductAssociationRepository.findBySku("sku")).thenReturn(productAssociation1);

        final ProductAssociation productAssociation2 = new ProductAssociation(0L, "sku", "relatedProducts",
                "bundleDeals", "productVariations");
        when(mockProductAssociationRepository.save(any(ProductAssociation.class))).thenReturn(productAssociation2);

        when(mockProductRepository.findById(0L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.updateProduct(productDto))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testDeleteProduct() {

        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findBySku("sku")).thenReturn(optional);

        final boolean result = productServiceImplUnderTest.deleteProduct("sku");

        assertThat(result).isTrue();
        verify(mockProductAssociationRepository).deleteById(0L);
        verify(mockProductRepository).deleteById(0L);
    }

    @Test
    void testDeleteProduct_ProductRepositoryFindBySkuReturnsAbsent() {

        when(mockProductRepository.findBySku("sku")).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.deleteProduct("sku"))
                .isInstanceOf(ResourceNotFoundException.class);
    }

    @Test
    void testSearchProducts_ProductRepositoryReturnsNoItems() {

        final ProductSearchCriteriaDto searchCriteria = new ProductSearchCriteriaDto();
        searchCriteria.setName("name");
        searchCriteria.setCategory("category");
        searchCriteria.setDescription("description");
        searchCriteria.setImage("image");
        searchCriteria.setSpecification("specification");
        searchCriteria.setSku("sku");
        searchCriteria.setBrand("brand");
        searchCriteria.setAmount(0L);

        when(mockProductRepository.findAll()).thenReturn(Collections.emptyList());

        final List<ProductDto> result = productServiceImplUnderTest.searchProducts(searchCriteria);

        assertThat(result).isEqualTo(Collections.emptyList());
    }

    @Test
    void testUpdatePrice() {

        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);


        final Product product = new Product();
        product.setProductId(0L);
        product.setName("name");
        product.setCategory("category");
        product.setDescription("description");
        product.setImage("image");
        product.setSpecification("specification");
        product.setSku("sku");
        product.setBrand("brand");
        final Price price = new Price();
        price.setAmount(0L);
        price.setDiscount(0L);
        product.setPrice(price);
        final ProductAssociation productAssociation = new ProductAssociation();
        productAssociation.setAssociationId(0L);
        productAssociation.setSku("sku");
        productAssociation.setRelatedProducts("relatedProducts");
        productAssociation.setBundleDeals("bundleDeals");
        productAssociation.setProductVariations("productVariations");
        product.setProductAssociation(productAssociation);
        final Optional<Product> optional = Optional.of(product);
        when(mockProductRepository.findById(0L)).thenReturn(optional);


        productServiceImplUnderTest.updatePrice(0L, priceDto);


        verify(mockPriceRepository).save(any(Price.class));
    }

    @Test
    void testUpdatePrice_ProductRepositoryReturnsAbsent() {

        final PriceDto priceDto = new PriceDto();
        priceDto.setAmount(0L);
        priceDto.setDiscount(0L);

        when(mockProductRepository.findById(0L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> productServiceImplUnderTest.updatePrice(0L, priceDto))
                .isInstanceOf(ResourceNotFoundException.class);
    }
}
