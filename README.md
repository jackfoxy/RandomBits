# RandomBits

**RandomBits** is a F# library consuming the [ANU Quantum Random Numbers Server](http://qrng.anu.edu.au/index.php) API, providing the following end-products:

Signed and unsigned 8, 16, 32, and 64 bit random numbers.

Random numbers constrained within a range.

Sequences of random numbers.

Sequences of random numbers constrained within a range.

Sequences of random numbers constrained within a range and each member of the sequence is unique.

## Purpose of RandomBits

Appropriate pseudo-random number generators are perfectly adequate for most random number generation use, and significantly faster than using **RandomBits**, especially because of network latency. **RandomBits** is primarily a demonstration project.