package com.flight_reservation_app.utilities;

import java.io.File;

import javax.mail.internet.MimeMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;

@Component
public class EmailUtil {

	@Autowired
	private JavaMailSender sender;
	
	public void sendItinerary(String toAddress, String filePath) {
	
		MimeMessage message = sender.createMimeMessage();
		try {
			MimeMessageHelper messagehelper = new MimeMessageHelper(message,true);
			messagehelper.setTo(toAddress);
			messagehelper.setSubject("Itinery Of Fligth");
			messagehelper.setText("Please find the attached");
			messagehelper.addAttachment("Itinerary", new File(filePath));
			sender.send(message);
		} catch (Exception e) {
			e.printStackTrace();
		}
	
	}
}
