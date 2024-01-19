package com.flight_reservation_app.service;




import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.flight_reservation_app.dto.ReservationRequest;
import com.flight_reservation_app.entity.Flight;
import com.flight_reservation_app.entity.Passenger;
import com.flight_reservation_app.entity.Reservation;
import com.flight_reservation_app.repository.FlightRepository;
import com.flight_reservation_app.repository.PassengerRepository;
import com.flight_reservation_app.repository.ReservationRepository;


@Service
public class ReservationServiceImpl implements ReservationService {
	
	@Autowired
	private PassengerRepository passengerRepo;
	
	@Autowired
	private FlightRepository flightRepo;
	
	@Autowired
	private ReservationRepository reservationRepo;

	@Override
	public Reservation bookFlight(ReservationRequest requst) {
		
		Passenger passenger = new Passenger();
		passenger.setFirstName(requst.getFirstName());
		passenger.setMiddleName(requst.getMiddleName());
		passenger.setLastName(requst.getLastName());
		passenger.setEmail(requst.getEmail());
		passenger.setPhone(requst.getPhone());
		passengerRepo.save(passenger);
		
		Long flightId = requst.getFlightId();
		Optional<Flight> findById = flightRepo.findById(flightId);
		Flight flight = findById.get();
		
		Reservation reservation = new Reservation();
		reservation.setFlight(flight);
		reservation.setPassenger(passenger);
		reservation.setCheckedIn(false);
		reservation.setNumberOfBags(0);
		reservationRepo.save(reservation);
		
		return reservation;
	}

}
