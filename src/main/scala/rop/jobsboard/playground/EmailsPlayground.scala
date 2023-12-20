package rop.jobsboard.playground

import cats.effect.{ExitCode, IO, IOApp}
import rop.jobsboard.config.EmailServiceConfig
import rop.jobsboard.core.LiveEmails

import java.util.Properties
import javax.mail.internet.MimeMessage
import javax.mail.{Authenticator, Message, PasswordAuthentication, Session, Transport}

object EmailsPlayground {

  def main(args: Array[String]): Unit = {

    // credentials from: https://ethereal.email/
    // config
    val host        = "smtp.ethereal.email"
    val port        = 587
    val username    = "micheal.stehr@ethereal.email"
    val password    = "9GTdVnx2EMqWBWumbu"
    val frontendUrl = "https://google.com"

    val token = "ABCDEFG123456789"

    // properties file
    val prop = new Properties()
    prop.put("mail.smtp.auth", true)
    prop.put("mail.smtp.starttls.enable", true)
    prop.put("mail.smtp.host", host)
    prop.put("mail.smtp.port", port)
    prop.put("mail.smtp.ssl.trust", host)

    // authentication
    val auth = new Authenticator {
      override def getPasswordAuthentication: PasswordAuthentication = new PasswordAuthentication(username, password)
    }

    // session
    val session = Session.getInstance(prop, auth)

    // email itself
    val subject = "Test email subject"
    val content =
      s"""
        |<div style="border: 1px solid black; padding: 20px; font-family: sans-serif; line-height: 2; font-size: 20px;">
        |   <h1>Corp: Password recovery</h1>
        |   <p>Your password recovery token is: $token</p>
        |   <p>
        |     Click <a href="$frontendUrl/login">here</a> to get back to the application
        |   </p>
        |   <p>From your Corp</p>
        |</div>
        |""".stripMargin

    // message = MIME message
    val message = new MimeMessage(session)
    message.setFrom("the.sender@gmail.com")
    message.setRecipients(Message.RecipientType.TO, "the.recipient@gmail.com")
    message.setSubject(subject)
    message.setContent(content, "text/html; charset=utf-8")

    // send
    Transport.send(message)
  }
}

object EmailsEffectPlayground extends IOApp.Simple {

  override def run: IO[Unit] = for {
    emails <- LiveEmails[IO](
      EmailServiceConfig("smtp.ethereal.email", 587, "micheal.stehr@ethereal.email", "9GTdVnx2EMqWBWumbu", "https://google.com")
    )
    _ <- emails.sendPasswordRecoveryEmail("the.sender@gmail.com", "the.recipient@gmail.com", "token_123456789")
  } yield ()
}
