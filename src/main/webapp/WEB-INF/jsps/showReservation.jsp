<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html>
<head> 
<meta charset="ISO-8859-1">
<title>Reservation Details</title>
</head>
<body>
	<h2>Flight Details</h2>
		Flight Number:${flight.flightNumber}</br>
		Operating Airlines:${flight.operatingAirlines}</br>
		Departure City:${flight.departureCity}</br>
		Arrival City:${flight.arrivalCity}</br>
		Departure Date:${flight.dateOfDeparture}</br>
		
	<h2>Enter Passenger Details</h2>
		<form action="confirmReservation" method="post">
			<pre>
				First Name<input type="text" name="firstName"/>
				Middle Name<input type="text" name="middleName"/>
				Last Name<input type="text" name="lastName"/>
				Email<input type="text" name="email"/>
				Phone<input type="text" name="phone"/>
				<input type="hidden" name="flightId" value="${flight.id}"/>
		<h2>Enter the payment details</h2>
				Name<input type="text" name="cardHolderName"/>
				Card Number<input type="text" name="cardNumber"/>
				CVV<input type="text" name="cvv"/>
				Expiry Date<input type="text" name="expiryDate"/>
				<input type="submit" name="completereservation"/>
			</pre>
		</form>

</body>
</html>