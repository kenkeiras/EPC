#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <assert.h>
#include "pHash.h"

#define EXIT_VALUE (0xFFFFFFFFFFFFFFFF)

/**
 * Reads the length of the upcoming image data.
 * Returns EXIT_VALUE on EOF, does not make any attempt to fix it if that
 * value comes from the file.
 *
 */
uint64_t read_length(FILE *f){
    uint64_t length;
    if (fread(&length, 8, 1, f) != 1){
        return EXIT_VALUE;
    }
    return length;
}


/**
 * Reads the length of data specified by the parameter.
 *
 */
void read_data(uint8_t data[], uint64_t length, FILE *f){
    unsigned int already_read = 0;
    while (already_read < length){
        int read_step = fread(&data[already_read], sizeof(uint8_t),
                              length - already_read, f);

        already_read += read_step;
    }
}


/**
 * Write phash to file.
 *
 */
void write_phash(uint64_t phash, FILE *f){
    fwrite(&phash, 8, 1, f);
    fflush(f);
}


/**
 * Call the pHash library.
 *
 */
uint64_t compute_phash(uint8_t data[], uint64_t length){
    ulong64 hash = 144;
    /* To be called from mkstemp, keep the last 6 characters as X's ! */
    char fname[] = {"tmp_epc_phash_XXXXXX"};
    int fd = mkstemp(fname);
    assert(fd != -1);

    /* Write the data to a file, the library operates over them :\ */
    unsigned int already_wrote = 0;
    while(already_wrote < length){
        unsigned int write_step = write(fd, &data[already_wrote],
                                        length - already_wrote);

        already_wrote += write_step;
    }
    close(fd);

    ph_dct_imagehash(fname, hash);

    unlink(fname);
    return hash;
}


int main(int argc, char **arg){
    uint64_t length = read_length(stdin);

    while (length != EXIT_VALUE){
        uint8_t  data[length];
        read_data(data, length, stdin);

        uint64_t phash = compute_phash(data, length);
        write_phash(phash, stdout);

        length = read_length(stdin);
    }

    return 0;
}
