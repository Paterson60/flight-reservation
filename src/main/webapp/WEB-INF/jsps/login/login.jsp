
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>Login - Flight Reservation</title>
	<style>
		body {
			font-family: Arial, sans-serif;
			margin: 0;
			padding: 0;
			background-image:url('flight2.jpg');
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

		input[type="text"],
		input[type="password"] {
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
	<h1>Login to Flight Reservation</h1>
	<form action="verifylogin" method="post">
	${error}
		<input type="text" id="emailId" name="emailId" placeholder="Email" required>
		<input type="password" id="password" name="password" placeholder="Password" required>
		<button type="submit">Login</button>
	</form>
</div>
</body>
</html>