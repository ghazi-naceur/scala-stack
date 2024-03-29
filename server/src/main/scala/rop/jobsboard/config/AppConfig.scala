package rop.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

final case class AppConfig(
    postgresConfig: PostgresConfig,
    emberConfig: EmberConfig,
    securityConfig: SecurityConfig,
    tokenConfig: TokenConfig,
    emailServiceConfig: EmailServiceConfig,
    stripeConfig: StripeConfig
) derives ConfigReader
