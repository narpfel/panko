#ifndef __PANKO_STDARG_H
#define __PANKO_STDARG_H

#define __STDC_VERSION_STDARG_H__ 202311L

typedef struct __panko_va_list_impl {
    unsigned int gp_offset;
    unsigned int fp_offset;
    char* overflow_arg_area;
    char* reg_save_area;
} va_list[1];

typedef va_list __gnuc_va_list;

#define va_start(ap, ...) ( \
    (ap)->gp_offset = __panko_gp_offset, \
    (ap)->fp_offset = 0, \
    (ap)->overflow_arg_area = __panko_overflow_arg_area, \
    (ap)->reg_save_area = __panko_reg_save_area, \
    (void)0 \
)

#define va_copy(dest, src) ((dest)[0] = (src)[0], (void)0)

#define va_arg(ap, type) ( \
    /* TODO: this needs adjustment when there are more classes than INTEGER and MEMORY */ \
    sizeof(type) <= 16 && (ap)->gp_offset <= 48 - ((sizeof(type) + 7) / 8) * 8 \
    ? \
    /* fetch from `reg_save_area` */ \
    ( \
        ((ap)->gp_offset += ((sizeof(type) + 7) / 8) * 8), \
        *(typeof(type) const*)((ap)->reg_save_area + (ap)->gp_offset - ((sizeof(type) + 7) / 8) * 8) \
    ) \
    : \
    /* fetch from `overflow_arg_area` */ \
    ( \
        ((ap)->overflow_arg_area += ((sizeof(type) + 7) / 8) * 8), \
        *(typeof(type) const*)((ap)->overflow_arg_area - ((sizeof(type) + 7) / 8) * 8) \
    ) \
)

#define va_end(ap) ((void)0)

#endif
