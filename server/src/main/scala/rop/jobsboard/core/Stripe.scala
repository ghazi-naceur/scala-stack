package rop.jobsboard.core

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
}
