package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price; import org.junit.jupiter.api.Test; import org.springframework.beans.factory.annotation.Autowired; import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest; import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
class PriceRepositoryTest {

    @Autowired
    private PriceRepository priceRepository;

    @Test
    void testSaveAndFindById() {
        Price price = new Price();
        price.setAmount(1200L);
        price.setDiscount(200L);
        Price savedPrice = priceRepository.save(price);

        Optional<Price> retrievedPrice = priceRepository.findById(savedPrice.getPriceId());
        assertThat(retrievedPrice).isPresent();
        assertThat(retrievedPrice.get().getAmount()).isEqualTo(1200);
        assertThat(retrievedPrice.get().getDiscount()).isEqualTo(200);
    }

    @Test
    void testDeleteById() {
        Price price = new Price();
        price.setAmount((long) 1200.00);
        price.setDiscount((long) 200.00);
        Price savedPrice = priceRepository.save(price);

        priceRepository.deleteById(savedPrice.getPriceId());
        Optional<Price> retrievedPrice = priceRepository.findById(savedPrice.getPriceId());
        assertThat(retrievedPrice).isNotPresent();
    }
}