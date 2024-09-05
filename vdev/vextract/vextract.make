EXEC= vextract

# Comment this out for Linux
#SOLARIS_64BIT_ARCH= -xarch=v9

CC= cc

CFLAGS= -DUSE_MD5 $(SOLARIS_64BIT_ARCH)

LDFLAGS= $(SOLARIS_64BIT_ARCH)

OBJS= des.o df_read.o dfparse.o vextract.o graphic.o image.o j_io.o j_string.o label.o md5c.o menu.o rbd_jpeg.o symbol.o text.o tre.o unblock.o write_fs.o write_text.o

LIBS= -lm

$(EXEC): $(OBJS)
	$(CC) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

clean:
	rm *.o

cleanall:
	rm $(OBJS) $(EXEC)
