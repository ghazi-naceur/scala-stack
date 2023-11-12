package rop.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

final case class PostgresConfig(nbThreads: Int, url: String, user: String, password: String) derives ConfigReader
