
play <- function(x, ...) {
	UseMethod("play")
}

play.Instrument <- function(x) {
	print("I am a Instrument")
}

play.Stringed <- function(x) {
	print("I am a Stringed")
}

play.default <- function(x) {
	print("I don't know who I am")
}

