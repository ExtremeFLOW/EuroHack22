FCLAGS  = `pkg-config --cflags neko`
LIBS    = `pkg-config --libs neko`
FC      = `pkg-config --variable=compiler neko`
INC_FLAGS = `pkg-config --cflags-only-I neko`

DEST    = bk5
SRC	= ax_bk5.f90 driver.f90
OBJ	= ${SRC:.f90=.o}

all: $(DEST)

install:

clean:
	-rm -f *.o core *.core $(OBJ) $(DEST)

$(DEST): cuda ${OBJ}
	$(FC) $(FCLAGS) ax_bk5_aux.o \
		${OBJ} -o $@  $(LIBS) -lcudart

cuda:
	nvcc -O3 ${INC_FLAGS} -c ax_bk5_aux.cu

%.o: %.f90
	${FC} ${FCLAGS} -c $<


