/* Quite a bit of code as taken/adapted from libcurl examples:
 * http://curl.askapache.com/c/getinmemory.html */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <curl/curl.h>
#include <arpa/inet.h>

/* Variable length strings are length-prefixed. */
struct pstring {
    size_t size;
    char *memory;
};

/**
 * Read the URL sent from the erlang side.
 *
 * @return The string sent from erlang, must be freed after used.
 */
char* read_url(){
    uint32_t length;
    if (fread(&length, 4, 1, stdin) != 1){
        return NULL;
    }
    length = ntohl(length);

    char *url = malloc(sizeof(char) * (length + 1));
    if (url == NULL){
        return NULL;
    }

    unsigned int already_read = 0;
    while (already_read < length){
        already_read += fread(&url[already_read], sizeof(uint8_t),
                              length - already_read, stdin);
    }
    url[length] = '\0';
    return url;
}

/**
 * Sends the result back to erlang.
 *
 * @param headers The length-prefixed string containg the response headers.
 * @param body    The lenght-prefixed body containing the response body.
 */
void show_result(struct pstring headers, struct pstring body){
    /* Output the headers */
    uint32_t headers_size = htonl(headers.size);
    fwrite(&headers_size, 4, 1, stdout);
    unsigned int written_head = 0;
    while (written_head < headers.size){
        written_head += fwrite(&(headers.memory[written_head]), sizeof(uint8_t),
                               headers.size - written_head, stdout);
    }
    fflush(stdout);

    /* Output the body */
    uint32_t body_size = htonl(body.size);
    fwrite(&body_size, 4, 1, stdout);
    unsigned int written_body = 0;
    while (written_body < body.size){
        written_body += fwrite(&(body.memory[written_body]), sizeof(uint8_t),
                               body.size - written_body, stdout);
    }
    fflush(stdout);
}

/**
 * Makes cURL capable of using strings instead of directly writting to files.
 * Manages the addition of contents to the string.
 *
 * @param ptr The pointer containing the new data.
 * @param size The size of `ptr' data type.
 * @param nmemb The number of members in `ptr'.
 * @param data  The data gathered along the requests.
 *
 * @return size_t
 */
static size_t write_memory_callback(void *ptr, size_t size,
                                    size_t nmemb, void *data){

    size_t new_data_size = size * nmemb;
    struct pstring *mem = (struct pstring *)data;

    mem->memory = realloc(mem->memory, mem->size + new_data_size + 1);
    if (mem->memory) {
        /* If reallocation goes OK, copy the remaining data */
        memcpy(&(mem->memory[mem->size]), ptr, new_data_size);
        mem->size += new_data_size;
        mem->memory[mem->size] = 0;
    }
    return new_data_size;
}

/**
 * Returns to erlang the contents of the requested URL.
 *
 * @param url The URL to read.
 */
void show_url(char *url){
    CURL *curl_handle;

    struct pstring chunk = {.memory = NULL, .size = 0};
    struct pstring headers = {.memory = NULL, .size = 0};

    /* init the curl session */
    curl_handle = curl_easy_init();

    /* specify URL to get */
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);

    /* send all data to this function  */
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);

    /* we pass our 'chunk' struct to the callback function */
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&chunk);

    /* we pass our 'chunk' struct to the callback function */
    curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, (void *)&headers);

    /* some servers don't like requests that are made without a user-agent
       field, so we provide one */
    curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "epc-crawler/0.1");

    /* get it! */
    curl_easy_perform(curl_handle);

    /* cleanup curl stuff */
    curl_easy_cleanup(curl_handle);

    /* Send the result to the port */
    show_result(headers, chunk);

    if(chunk.memory){
        free(chunk.memory);
    }
    if(headers.memory){
        free(headers.memory);
    }
}


int main(int argc, char **argv){
    curl_global_init(CURL_GLOBAL_ALL);

    char *url = read_url();

    while(url != NULL) {
        show_url(url);
        free(url);

        url = read_url();
    }
    return 0;
}
