C SUBROUTINE TO GET PHYSICAL MEMORY SIZE
        SUBROUTINE GET_MEM_SIZE(MEMSIZE)
        INTEGER MEMSIZE
        REAL    PMEMORY
        INTEGER*4 I, K
	CALL XVPARM('PMEM',PMEMORY,I,K,0) ! GET AMOUNT OF PHYSICAL MEMORY.
        MEMSIZE = PMEMORY*1024*1024       ! CONVERT MEGABYTES TO BYTES
        RETURN
        END

C SUBROUTINE TO EXPLAIN WHY LGEOM CAN'T HANDLE IMAGE.
        SUBROUTINE TOOBIG
	CALL MABEND('** PICTURE IS TOO LARGE FOR AVAILABLE MEMORY')
        RETURN
        END

C SUBROUTINE TO CHECK FOR SMALL PAGE FILE QUOTA UNDER VMS
        SUBROUTINE CHECK_VMS_PAGE_FILE_QUOTA  !the non-VMS VERSION
        RETURN                                !is a dummy.
        END
