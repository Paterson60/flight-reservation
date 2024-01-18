<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="ISO-8859-1">
<title>Flight And Reservation Details</title>
</head>
<body>
<h2>Flight And Reservation Details</h2>
Flight Number : ${flight.flightNumber }<br/>
Operating Airlines : ${flight.operatingAirlines }<br/>
Departure City : ${flight.departureCity }<br/>
Arrival City : ${flight.arrivalCity }<br/>
Date Of Departure : ${flight.dateOfDeparture }<br/>
Estimated Departure Time : ${flight.estimatedDepartureTime }<br/>

<h2>Enter Passenger Details</h2>
<form action="completeReservation" method="post">
<pre>
First Name <input type="text" name="firstName"/>
Last Name <input type="text" name="lastName"/>
Middle Name <input type="text" name="middleName"/>
Email Id <input type="text" name="email"/>
Phone <input type="text" name="phone"/>
<input type="hidden" name="flightId" value="${flight.id}">
<input type="Submit" name="Complete Reservation"/>
</pre>
</form>
</body>
</html>