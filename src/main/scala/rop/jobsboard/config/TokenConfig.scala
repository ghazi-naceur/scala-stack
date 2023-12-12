package rop.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

case class TokenConfig(tokenDuration: Long) derives ConfigReader
