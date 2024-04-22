<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>Reservation Details</title>
	<style>
		body {
			font-family: Arial, sans-serif;
			margin: 0;
			padding: 0;
			background-image:url('flight1.jpg');
			background-size: cover;
			background-position: center;
			display: flex;
			justify-content: center;
			align-items: center;
			height: 100vh;
		}

		.container {
			width: 400px;
			background-color: #fff;
			padding: 40px;
			border-radius: 10px;
			box-shadow: 0 0 20px rgba(0, 0, 0, 0.1);
		}

		h2 {
			margin-bottom: 20px;
			color: #007bff;
		}

		p {
			margin-bottom: 10px;
		}

		input[type="text"] {
			width: calc(100% - 20px);
			padding: 10px;
			margin-bottom: 20px;
			border: 1px solid #ccc;
			border-radius: 5px;
			box-sizing: border-box;
		}

		input[type="submit"] {
			width: 100%;
			padding: 10px;
			background-color: #007bff;
			color: #fff;
			border: none;
			border-radius: 5px;
			cursor: pointer;
			transition: background-color 0.3s ease;
		}

		input[type="submit"]:hover {
			background-color: #0056b3;
		}
	</style>
</head>
<body>
<div class="container">
	<h2>Flight Details</h2>
	<p>Flight Number: ${flight.flightNumber}</p>
	<p>Operating Airlines: ${flight.operatingAirlines}</p>
	<p>Departure City: ${flight.departureCity}</p>
	<p>Arrival City: ${flight.arrivalCity}</p>
	<p>Departure Date: ${flight.dateOfDeparture}</p>

	<h2>Enter Passenger Details</h2>
	<form action="confirmReservation" method="post">
		<input type="text" name="firstName" placeholder="First Name" required>
		<input type="text" name="middleName" placeholder="Middle Name">
		<input type="text" name="lastName" placeholder="Last Name" required>
		<input type="text" name="email" placeholder="Email" required>
		<input type="text" name="phone" placeholder="Phone" required>
		<input type="hidden" name="flightId" value="${flight.id}">

		<h2>Enter the payment details</h2>
		<input type="text" name="cardHolderName" placeholder="Name" required>
		<input type="text" name="cardNumber" placeholder="Card Number" required>
		<input type="text" name="cvv" placeholder="CVV" required>
		<input type="text" name="expiryDate" placeholder="Expiry Date" required>

		<input type="submit" name="completeReservation" value="Complete Reservation">
	</form>
</div>
</body>
</html>