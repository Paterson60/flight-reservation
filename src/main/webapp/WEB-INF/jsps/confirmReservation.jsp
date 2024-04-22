<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>Confirmation</title>
	<style>
		body {
			font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
			background-image:url('flight1.jpg');
			background-size: cover;
			background-position: center;
			margin: 0;
			padding: 0;
			display: flex;
			justify-content: center;
			align-items: center;
			height: 100vh;
		}

		.container {
			text-align: center;
			background-color: #fff;
			border-radius: 10px;
			box-shadow: 0 0 20px rgba(0, 0, 0, 0.1);
			padding: 40px;
			animation: fadeIn 0.5s ease;
			max-width: 400px;
			width: 100%;
		}

		h1 {
			color: #007bff;
			margin-bottom: 30px;
			font-size: 32px;
			font-weight: bold;
		}

		p {
			font-size: 18px;
			color: #444;
			line-height: 1.6;
			margin-bottom: 20px;
		}

		.confirmation-info {
			font-size: 24px;
			font-weight: bold;
			border: 2px solid transparent;
			padding: 10px 20px;
			border-radius: 8px;
			display: inline-block;
			transition: background-color 0.3s, color 0.3s, border-color 0.3s;
		}

		.confirmation-info:hover {
			background-color: #007bff;
			color: #fff;
			border-color: #007bff;
		}

		.additional-info {
			font-size: 16px;
			color: #777;
		}

		.enjoy-message {
			font-size: 24px;
			color: #28a745;
			font-weight: bold;
			margin-top: 40px;
			animation: pulse 1s infinite alternate;
		}

		@keyframes fadeIn {
			from {
				opacity: 0;
				transform: translateY(-20px);
			}
			to {
				opacity: 1;
				transform: translateY(0);
			}
		}

		@keyframes pulse {
			from {
				transform: scale(1);
			}
			to {
				transform: scale(1.1);
			}
		}
	</style>
</head>
<body>
<div class="container">
	<h1>Reservation Confirmed!</h1>
	<p>Your ticket has been successfully booked.</p>
	<p><span class="additional-info">Reservation ID:</span> <span class="confirmation-info">${reservationId}</span></p>
	<p class="enjoy-message">Enjoy your journey!</p>
</div>
</body>
</html>