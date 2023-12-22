package rop.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

final case class EmailServiceConfig(
    host: String,
    port: Int,
    username: String,
    password: String,
    frontendUrl: String
) derives ConfigReader
