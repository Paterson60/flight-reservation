package com.flight_reservation_app.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.flight_reservation_app.entity.User;
import com.flight_reservation_app.repository.UserRepository;

@Controller
public class UserController {
	
	@Autowired
	UserRepository userRepo;
	
	@RequestMapping("/showLoginPage")
	public String showLoginPage() {
		return"login/login";
	}
		
	@RequestMapping("/showReg")
	public String showReg() {
		return"login/showReg";
	}
	
	@RequestMapping("/saveReg")
	public String saveReg(@ModelAttribute("user") User user) {
		userRepo.save(user);
		return"login/login";
	}
	
	@RequestMapping("/verifyLogin")
	public String verifyLogin(@RequestParam("emailId") String emailId, @RequestParam("password") String password,ModelMap modelMap) {
//		System.out.println(emailId);
		User user = userRepo.findByEmail(emailId);
		if(user!=null) {
		if(user.getEmail().equals(emailId) && user.getPassword().equals(password)) {
			return"findFlights"; 
		}else {
			modelMap.addAttribute("error", "Invalid user/password");
			return"login/login";
		}
		}else {
			modelMap.addAttribute("error", "Invalid user/password");
			return"login/login";	
			
		}
	}

}
