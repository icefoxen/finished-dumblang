# Generate a pathological Dumblang program, to test the compiler's speed
# and such.

count = 5000
prolog = "func main (int):\nvar a int <- 10\n"
epilog = "0\nend\n"
statement = "a <- %d\n"

f = open( 'patho.dl', 'w' )

f.write( prolog )
for x in range( count ):
   f.write( statement % x )

f.write( epilog )
f.close()
