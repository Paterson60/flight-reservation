<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
 <%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>Display Flights</title>
	<style>
		body {
			font-family: Arial, sans-serif;
			margin: 0;
			padding: 0;
			background-image:url('flight1.jpg');
			background-size: cover;
			background-position: center;
		}

		h2 {
			margin-bottom: 20px;
			color: #007bff;
		}

		table {
			width: 100%;
			border-collapse: collapse;
			border: 1px solid #ccc;
		}

		th, td {
			padding: 10px;
			border: 1px solid #ccc;
		}

		th {
			background-color: #f2f2f2;
		}

		tr:hover {
			background-color: #f2f2f2;
		}

		a {
			text-decoration: none;
			color: #007bff;
		}

		a:hover {
			text-decoration: underline;
		}
	</style>
</head>
<body>
<h2>List of Flights</h2>
<table>
	<thead>
	<tr>
		<th>Airlines</th>
		<th>Departure</th>
		<th>Arrival City</th>
		<th>Departure City</th>
		<th>Select Flight</th>
	</tr>
	</thead>
	<tbody>
	<c:forEach items="${findFlight}" var="findFlight">
				<tr>
					<td>${findFlight.operatingAirlines}</td>
					<td>${findFlight.departureCity}</td>
					<td>${findFlight.arrivalCity}</td>
					<td>${findFlight.estimatedDepartureTime}</td>
					<td><a href="showCompleteReservation?flightId=${findFlight.id}">Select</a></td>
				</tr>
			</c:forEach>
	</tbody>
</table>
</body>
</html>