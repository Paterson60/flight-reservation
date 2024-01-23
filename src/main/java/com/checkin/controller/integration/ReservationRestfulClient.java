package com.checkin.controller.integration;

import org.springframework.web.bind.annotation.RequestBody;

import com.checkin.dto.Reservation;
import com.checkin.dto.ReservationUpdateRequest;


public interface ReservationRestfulClient {
	
	public Reservation findReservation(Long id);
	public Reservation updateReservation(@RequestBody ReservationUpdateRequest request);	

}
