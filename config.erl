{bind, ["127.0.0.1", 1812]}.
{ras, ["127.0.0.1", 1812, "secret"]}.
{realm, ["domain1.com", optional]}.
{realm, ["domain2.com", required]}.
{realm, ["domain3.com", required]}.
{somewrongconfig, [somestuff]}.
