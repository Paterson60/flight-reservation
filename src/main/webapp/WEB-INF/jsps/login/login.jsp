<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html>
<html>
<head>
<meta charset="ISO-8859-1">
<title>Login</title>
</head>
<body>
<h2>Login Page</h2>
${error}
<form action="verifyLogin" method="post">
Email <input type="text" name="emailId"/>
Password <input type="text" name="password"/>
<input type="submit" value="login"/>
</form>

</body>
</html>