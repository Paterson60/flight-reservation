<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>

<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>Find Flights - Flight Reservation</title>
	<style>
		body {
			font-family: Arial, sans-serif;
			margin: 0;
			padding: 0;
			background-image:url('flight1.jpg');
			background-size: cover;
			background-position: center;
		}

		.container {
			width: 360px;
			margin: 100px auto;
			background-color: #fff;
			padding: 40px;
			border-radius: 8px;
			box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
		}

		h1 {
			text-align: center;
			margin-bottom: 30px;
		}

		label {
			display: block;
			margin-bottom: 10px;
		}

		input[type="text"],
		input[type="date"] {
			width: 100%;
			padding: 10px;
			margin-bottom: 20px;
			border: 1px solid #ccc;
			border-radius: 5px;
		}

		button {
			width: 100%;
			padding: 10px;
			background-color: #007bff;
			color: #fff;
			border: none;
			border-radius: 5px;
			cursor: pointer;
		}

		button:hover {
			background-color: #0056b3;
		}
	</style>
</head>
<body>
<div class="container">
	<h1>Find Flights</h1>
	<form action="findFlight">
		<label for="from">From:</label>
		<input type="text" id="from" name="from" placeholder="Departure City" required>

		<label for="to">To:</label>
		<input type="text" id="to" name="to" placeholder="Destination City" required>

		<label for="date">Date:</label>
		<input type="text" id="date" name="departureDate" required>

		<button type="submit">Search Flights</button>
	</form>
</div>
</body>
</html>