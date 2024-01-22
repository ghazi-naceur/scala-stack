package rop.jobsboard.core

import cats.MonadThrow
import cats.implicits.*
import com.stripe.Stripe as TheStripe
import com.stripe.model.checkout.Session
import com.stripe.param.checkout.SessionCreateParams
import org.typelevel.log4cats.Logger
import rop.jobsboard.config.StripeConfig
import rop.jobsboard.logging.syntax.*

trait Stripe[F[_]] {

  /*
      1- Someone calls the payment endpoint on our server: For example after clicking on our Post Job button
          (Send a JonInfo to us .. It will be persisted in the DB, otherwise it's going to be lost)
          ==> Job[F].create( ..., active = true) , once the webhook comes back to us
      2- Return a checkout page URL: we will automatically render a checkout page url
      3- The frontend will automatically redirect the frontend to that url
      4- The user pays (Fills credit card details...)
      5- The backend will be notified by stripe (through a webhook)
          - Test mode: Use Stripe CLI to redirect the events to localhost:4041/api/jobs/webhook
      6- Perform the final operation on the job advert - Set the active flag back to false for that Job Id

              App => HTTP => Stripe => redirect user
                                                    <= user pays Stripe
           activate job <=  webhook <= Stripe
   */

  def createCheckoutSession(jobId: String, usrEmail: String): F[Option[Session]]
}

class LiveStripe[F[_]: MonadThrow: Logger](key: String, price: String, successUrl: String, cancelUrl: String) extends Stripe[F] {

  // Globally set constant
  TheStripe.apiKey = key
  override def createCheckoutSession(jobId: String, userEmail: String): F[Option[Session]] =
    SessionCreateParams
      .builder()
      .setMode(SessionCreateParams.Mode.PAYMENT)
      .setInvoiceCreation(
        SessionCreateParams.InvoiceCreation.builder().setEnabled(true).build()
      )
      .setPaymentIntentData(
        SessionCreateParams.PaymentIntentData.builder().setReceiptEmail(userEmail).build()
      )
      .setSuccessUrl(s"$successUrl/$jobId")
      .setCancelUrl(cancelUrl)
      .setCustomerEmail(userEmail)
      .setClientReferenceId(jobId) // unique id that's sent back to us from the webhook, so we can reference back the order that was placed when the user started to checkout session
      .addLineItem(
        SessionCreateParams.LineItem
          .builder()
          .setQuantity(1L)
          .setPrice(price)
          .build()
      )
      .build()
      .pure[F]
      .map(params => Session.create(params))
      .map(_.some)
      .logError(error => s"Creating checkout session failed: $error")
      .recover { case _ => None }
}

object LiveStripe {

  // See stripe doc: https://stripe.com/docs/checkout/quickstart?lang=java
  def apply[F[_]: MonadThrow: Logger](stripeConfig: StripeConfig): F[LiveStripe[F]] =
    new LiveStripe[F](stripeConfig.key, stripeConfig.price, stripeConfig.successUrl, stripeConfig.cancelUrl).pure[F]
}
