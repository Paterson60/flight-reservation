package com.checkin.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.checkin.controller.integration.ReservationRestfulClient;
import com.checkin.dto.Reservation;
import com.checkin.dto.ReservationUpdateRequest;


@Controller
public class ReservationController {
	
	@Autowired
	private ReservationRestfulClient restClient;

	@RequestMapping("/startCheckIn")
	public String startCheckIn() {
		return"startCheckIn";
	}
	
	//this calls the reservationResfulclientimpl
	@RequestMapping("/proceedCheckIn")
	public String proceedCheckIn(@RequestParam("id") Long id,ModelMap modelMap) {
		Reservation reservation = restClient.findReservation(id);
		modelMap.addAttribute("reservation", reservation);
		return"displayReservation";
	}
	
	@RequestMapping("/proceedToCheckIn")
	public String proceedToCheckIn(@RequestParam("id") Long id,@RequestParam("numberOfBags") int numberOfBags) {
		ReservationUpdateRequest request = new ReservationUpdateRequest();
		request.setId(id);
		request.setNumberOfBags(numberOfBags);
		request.setCheckedIn(true);
		restClient.updateReservation(request);
		return"cofirmReservation";
	}
}
