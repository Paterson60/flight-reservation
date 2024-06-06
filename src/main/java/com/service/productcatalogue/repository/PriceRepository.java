package com.service.productcatalogue.repository;

import com.service.productcatalogue.entity.Price;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PriceRepository extends JpaRepository<Price,Long> {

    Price findByPriceId(Long priceId);

}
