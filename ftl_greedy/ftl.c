// Copyright 2011 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// GreedyFTL source file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// - support POR
//  + fixed metadata area (Misc. block/Map block)
//  + logging entire FTL metadata when each ATA commands(idle/ready/standby) was issued
//

#include "jasmine.h"

//----------------------------------
// macro
//----------------------------------
#define VC_MAX 0xCDCD
#define MISCBLK_VBN 0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK (((PAGE_MAP_BYTES / NUM_BANKS) + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE)
#define META_BLKS_PER_BANK (1 + 1 + MAPBLKS_PER_BANK) // include block #0, misc block

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)

//----------------------------------
// metadata structure
//----------------------------------
typedef struct _ftl_statistics
{
    UINT32 gc_cnt;
    UINT32 page_wcount; // page write count
} ftl_statistics;

typedef struct _misc_metadata
{
    UINT32 cur_write_vpn;                         // physical page for new write
    UINT32 cur_miscblk_vpn;                       // current write vpn for logging the misc. metadata
    UINT32 cur_mapblk_vpn[MAPBLKS_PER_BANK];      // current write vpn for logging the age mapping info.
    UINT32 gc_vblock;                             // vblock number for garbage collection
    UINT32 free_blk_cnt;                          // total number of free block count
    UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC

    UINT32 num_random_blk;
} misc_metadata; // per bank

typedef struct _random
{
    UINT32 random_blk_list[NUM_RANDOM_BLK + NUM_OP_BLK];
    UINT32 max_num_random_blk;
} random;

static random g_random;

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32 g_bad_blk_count[NUM_BANKS];
// SATA read/write buffer pointer id
UINT32 g_ftl_read_buf_id;
UINT32 g_ftl_write_buf_id;

//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 ~ #31: page mapping table
// block #32: a free block for gc
// block #33~: user data blocks

//----------------------------------
// macro functions
//----------------------------------
// #define is_full_all_blks(bank)  (g_misc_meta[bank].free_blk_cnt == 1)
#define is_full_all_blks(bank) (g_misc_meta[bank].free_blk_cnt == 0)

#define inc_full_blk_cnt(bank) (g_misc_meta[bank].free_blk_cnt--)
#define dec_full_blk_cnt(bank) (g_misc_meta[bank].free_blk_cnt++)
#define inc_mapblk_vpn(bank, mapblk_lbn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank) (g_misc_meta[bank].cur_miscblk_vpn++)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn) ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank) (g_bad_blk_count[bank])
#define get_cur_write_vpn(bank) (g_misc_meta[bank].cur_write_vpn)
#define set_new_write_vpn(bank, vpn) (g_misc_meta[bank].cur_write_vpn = vpn)
#define get_gc_vblock(bank) (g_misc_meta[bank].gc_vblock)
#define set_gc_vblock(bank, vblock) (g_misc_meta[bank].gc_vblock = vblock)
#define set_lpn(bank, page_num, lpn) (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num] = lpn)
#define get_lpn(bank, page_num) (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num])
#define get_miscblk_vpn(bank) (g_misc_meta[bank].cur_miscblk_vpn)
#define set_miscblk_vpn(bank, vpn) (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define get_mapblk_vpn(bank, mapblk_lbn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn])
#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
#define CHECK_LPAGE(lpn) ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn) ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void format(void);
static void write_format_mark(void);
static void sanity_check(void);
static void load_pmap_table(void);
static void load_misc_metadata(void);
static void init_metadata_sram(void);
static void load_metadata(void);
static void logging_pmap_table(void);
static void logging_misc_metadata(void);
static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
static void set_vpn(UINT32 const lpn, UINT32 const vpn);
static void garbage_collection(UINT32 const bank);
static void set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static BOOL32 check_format_mark(void);
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock);
static UINT32 get_vpn(UINT32 const lpn);
static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank);

//----------------------------------
// ZNS
//----------------------------------
UINT32 open_zone_count = 0;
UINT32 max_vblock_zone;

int fbg_queue_front;
int fbg_queue_back;
void inc_fbg_queue_front()
{
    fbg_queue_front++;
    if (fbg_queue_front >= MAX_ZONE)
    {
        fbg_queue_front = max_vblock_zone;
    }
}
void inc_fbg_queue_back()
{
    fbg_queue_back++;
    if (fbg_queue_back >= MAX_ZONE)
        fbg_queue_back = max_vblock_zone;
}

UINT32 b2z[6];

UINT32 get_zone_desc(UINT32 zone, zone_desc_elem elem)
{
    return _read_dram_32(ZONE_DESC_ADDR + ((zone) * sizeof(struct zone_desc) + (elem) * sizeof(int)));
}
void set_zone_desc(UINT32 zone, zone_desc_elem elem, UINT32 val)
{
    _write_dram_32(ZONE_DESC_ADDR + ((zone) * sizeof(struct zone_desc) + (elem) * sizeof(int)), val);
}
UINT32 get_z2f_entry(UINT32 zone, zone_desc_elem elem)
{
    return _read_dram_32(Z2F_ENTRY_ADDR + ((zone) * sizeof(struct z2f_entry) + (elem) * sizeof(int)));
}
void set_z2f_entry(UINT32 zone, zone_desc_elem elem, UINT32 val)
{
    _write_dram_32(Z2F_ENTRY_ADDR + ((zone) * sizeof(struct z2f_entry) + (elem) * sizeof(int)), val);
}
UINT32 get_fbg_queue(UINT32 index)
{
    return _read_dram_32(FBG_QUEUE_ADDR + ((index) * sizeof(int)));
}
void set_fbg_queue(UINT32 index, UINT32 val)
{
    _write_dram_32(FBG_QUEUE_ADDR + ((index) * sizeof(int)), val);
}
UINT32 is_random_lba(UINT32 lba)
{
    if (lba < (ZONE_SIZE * RANDOM_AREA_ZONE)) // 393216
    {
        return 1;
    }
    return 0;
}

static void sanity_check(void)
{
#ifdef DEBUG
    // uart_printf("[+] greedy sanity_check");
#endif

    // UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
    //     + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES;
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES + ZONE_BUF_BYTES + Z2F_ENTRY_BYTES + ZONE_DESC_BYTES + FBG_QUEUE_BYTES;
    //+ ZONE_BUF_BYTES;

    if ((dram_requirement > DRAM_SIZE) ||         // DRAM metadata size check
        (sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
    {
        led_blink();
        while (1)
            ;
    }

#ifdef DEBUG
    // uart_printf("[-] greedy sanity_check");
#endif
}

static void build_bad_blk_list(void)
{
    UINT32 bank, num_entries, result, vblk_offset;
    scan_list_t *scan_list = (scan_list_t *)TEMP_BUF_ADDR;

    mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

    disable_irq();

    flash_clear_irq();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
        SETREG(FCP_BANK, REAL_BANK(bank));
        SETREG(FCP_OPTION, FO_E);
        SETREG(FCP_DMA_ADDR, (UINT32)scan_list);
        SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
        SETREG(FCP_COL, 0);
        SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
        SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

        SETREG(FCP_ISSUE, NULL);
        while ((GETREG(WR_STAT) & 0x00000001) != 0)
            ;
        while (BSP_FSM(bank) != BANK_IDLE)
            ;

        num_entries = NULL;
        result = OK;

        if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
        {
            result = FAIL;
        }
        else
        {
            UINT32 i;

            num_entries = read_dram_16(&(scan_list->num_entries));

            if (num_entries > SCAN_LIST_ITEMS)
            {
                result = FAIL;
            }
            else
            {
                for (i = 0; i < num_entries; i++)
                {
                    UINT16 entry = read_dram_16(scan_list->list + i);
                    UINT16 pblk_offset = entry & 0x7FFF;

                    if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
                    {
#if OPTION_REDUCED_CAPACITY == FALSE
                        result = FAIL;
#endif
                    }
                    else
                    {
                        write_dram_16(scan_list->list + i, pblk_offset);
                    }
                }
            }
        }

        if (result == FAIL)
        {
            num_entries = 0; // We cannot trust this scan list. Perhaps a software bug.
        }
        else
        {
            write_dram_16(&(scan_list->num_entries), 0);
        }

        g_bad_blk_count[bank] = 0;

        for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
        {
            BOOL32 bad = FALSE;

#if OPTION_2_PLANE
            {
                UINT32 pblk_offset;

                pblk_offset = vblk_offset * NUM_PLANES;

                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }

                pblk_offset = vblk_offset * NUM_PLANES + 1;

                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }
            }
#else
            {
                // fix bug@jasmine v.1.1.0
                if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
                {
                    bad = TRUE;
                }
            }
#endif

            if (bad)
            {
                g_bad_blk_count[bank]++;
                set_bit_dram(BAD_BLK_BMP_ADDR + bank * (VBLKS_PER_BANK / 8 + 1), vblk_offset);
            }
        }
    }
}

void ftl_open(void)
{
#ifdef DEBUG
    // uart_printf("[+] greedy ftl_open");
#endif

    // debugging example 1 - use breakpoint statement!
    /* *(UINT32*)0xFFFFFFFE = 10; */

    /* UINT32 volatile g_break = 0; */
    /* while (g_break == 0); */

    led(0);
    sanity_check();
    //----------------------------------------
    // read scan lists from NAND flash
    // and build bitmap of bad blocks
    //----------------------------------------
    build_bad_blk_list();

    //----------------------------------------
    // If necessary, do low-level format
    // format() should be called after loading scan lists, because format() calls is_bad_block().
    //----------------------------------------
    /* 	if (check_format_mark() == FALSE) */
    if (TRUE)
    {
        format();
    }
    // load FTL metadata
    else
    {
        load_metadata();
    }
    g_ftl_read_buf_id = 0;
    g_ftl_write_buf_id = 0;

    // b2z init
    for (int i = 0; i < MAX_OPEN_ZONE; i++)
    {
        b2z[i] = TOTAL_ZONE;
    }

    // This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
    flash_clear_irq();

    SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
    SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

    enable_irq();

#ifdef DEBUG
    // uart_printf("[-] greedy ftl_open");
#endif
}
void ftl_flush(void)
{
    /* ptimer_start(); */
    logging_pmap_table();
    logging_misc_metadata();
    /* ptimer_stop_and_uart_print(); */
}
// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
    ASSERT(lba + num_sectors <= NUM_LSECTORS);
    ASSERT(num_sectors > 0);

    ftl_write(lba, num_sectors);
}
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
#ifdef DEBUG
    uart_printf("[+] greedy ftl_read");
#endif

    UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, vpn;

    lpn = lba / SECTORS_PER_PAGE;
    sect_offset = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    int zone = lba_to_zone(lba);
#ifdef DEBUG
    uart_printf("-> zone: %d", zone);
#endif

    if (is_random_lba(lba))
    {
#ifdef DEBUG
        uart_printf("-> ftl_read from random");
#endif
        while (remain_sects != 0)
        {
            int lpn_in_zone = lpn % (NUM_BANKS * PAGES_PER_BLK);
            int bank_offset = lpn_in_zone % NUM_BANKS;
            int bank = bank_offset;
            int block = get_z2f_entry(zone, ZE_FBG);
            int page = lpn_in_zone / NUM_BANKS;

            if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
            {
                num_sectors_to_read = remain_sects;
            }
            else
            {
                num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
            }
            bank = get_num_bank(lpn); // page striping
            vpn = get_vpn(lpn);
            CHECK_VPAGE(vpn);

#ifdef DEBUG
            uart_printf("bank : %d, vpn : %d, lpn : %d, zone : %d ", bank, vpn, lpn, zone);
#endif

            if (vpn != NULL)
            {
                nand_page_ptread_to_host(bank,
                                         vpn / PAGES_PER_BLK,
                                         vpn % PAGES_PER_BLK,
                                         sect_offset,
                                         num_sectors_to_read);
            }
            // The host is requesting to read a logical page that has never been written to.
            else
            {
                UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

#if OPTION_FTL_TEST == 0
                while (next_read_buf_id == GETREG(SATA_RBUF_PTR))
                    ; // wait if the read buffer is full (slow host)
#endif

                // fix bug @ v.1.0.6
                // Send 0xFF...FF to host when the host request to read the sector that has never been written.
                // In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
                // However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
                mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset * BYTES_PER_SECTOR,
                             0xFFFFFFFF, num_sectors_to_read * BYTES_PER_SECTOR);

                flash_finish();

                SETREG(BM_STACK_RDSET, next_read_buf_id); // change bm_read_limit
                SETREG(BM_STACK_RESET, 0x02);             // change bm_read_limit

                g_ftl_read_buf_id = next_read_buf_id;
            }
            sect_offset = 0;
            remain_sects -= num_sectors_to_read;
            lpn++;
        }
#ifdef DEBUG
        uart_printf("[-] greedy ftl_read");
#endif
        return;
    }

#ifdef DEBUG
    uart_printf("-> ftl_read from sequential");
#endif

    int fbg = get_z2f_entry(zone, ZE_FBG);
    int start_lba = lba;
    int end_lba = start_lba + num_sectors - 1;
    int end_written_lba = get_zone_desc(zone, ZD_WP) - 1;
    int real_end_lba = end_lba > end_written_lba ? end_written_lba : end_lba;

    int start_lpn = start_lba / SECTORS_PER_PAGE;
    int real_end_lpn = real_end_lba / SECTORS_PER_PAGE;
    int end_lpn = end_lba / SECTORS_PER_PAGE;

    int sector_acc = 0;

    for (int lpn = start_lpn; lpn <= end_lpn; lpn++)
    {
        int start_sector = lpn == start_lpn ? start_lba % SECTORS_PER_PAGE : 0;
        int real_end_sector = lpn == end_lpn ? real_end_lba % SECTORS_PER_PAGE : SECTORS_PER_PAGE - 1;
        int end_sector = lpn == end_lpn ? end_lba % SECTORS_PER_PAGE : SECTORS_PER_PAGE - 1;
        int real_n_sector = real_end_sector - start_sector + 1;
        int n_sector = end_sector - start_sector + 1;

        // zone empty
        if (get_zone_desc(zone, ZD_STATE) == ZONE_EMPTY)
        {
#ifdef DEBUG
            uart_printf("-> zone empty");
#endif
            UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

#if OPTION_FTL_TEST == 0
            while (next_read_buf_id == GETREG(SATA_RBUF_PTR))
                ; // wait if the read buffer is full (slow host)
#endif

            mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + start_sector * BYTES_PER_SECTOR,
                         0xFFFFFFFF, n_sector * BYTES_PER_SECTOR);

            flash_finish();

            SETREG(BM_STACK_RDSET, next_read_buf_id); // change bm_read_limit
            SETREG(BM_STACK_RESET, 0x02);             // change bm_read_limit

            g_ftl_read_buf_id = next_read_buf_id;
            sector_acc += n_sector;
            continue;
        }

        // find buffer for current zone
        UINT32 buffer_idx = TOTAL_ZONE;
        for (int i = 0; i < MAX_OPEN_ZONE; i++)
        {
            if (b2z[i] == zone)
            {
                buffer_idx = i;
                break;
            }
        }
        ASSERT(buffer_idx != TOTAL_ZONE);
#ifdef DEBUG
        if (buffer_idx == TOTAL_ZONE)
            uart_printf("-> [XXXXX] read");
#endif

        if (lpn == real_end_lpn)
        {
#ifdef DEBUG
            uart_printf("-> read from buffer");
#endif

            // read from buffer
            UINT32 wp_offset = get_zone_desc(zone, ZD_WP) % SECTORS_PER_PAGE;

            // sata read buffer 조정
            UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

#if OPTION_FTL_TEST == 0
            while (next_read_buf_id == GETREG(SATA_RBUF_PTR))
                ; // wait if the read buffer is full (slow host)
#endif

            // fill all with 0xFF
            if (wp_offset <= start_sector)
            {
#ifdef DEBUG
                uart_printf("-> if (wp_offset <= start_sector)");
#endif
                mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + start_sector * BYTES_PER_SECTOR,
                             0xFFFFFFFF, n_sector * BYTES_PER_SECTOR);
            }
            // read all from buffer
            else if (end_sector < wp_offset)
            {

#ifdef DEBUG
                uart_printf("-> else if (end_sector < wp_offset)");
#endif
                mem_copy(RD_BUF_PTR(g_ftl_read_buf_id) + start_sector * BYTES_PER_SECTOR,
                         ZONE_BUF(buffer_idx) + start_sector * BYTES_PER_SECTOR,
                         n_sector * BYTES_PER_SECTOR);
            }
            // half from buffer, half fill with 0xFF
            else
            {
#ifdef DEBUG
                uart_printf("-> else (half from buffer, half filled with 0xFF)");
#endif
                // uart_printf("-> else (half from buffer, half filled with 0xFF)");
                // UINT32 n_valid_sector = wp_offset - start_sector + 1;
                UINT32 n_valid_sector = wp_offset - start_sector;
                UINT32 n_invalid_sector = n_sector - n_valid_sector;
                mem_copy(RD_BUF_PTR(g_ftl_read_buf_id) + start_sector * BYTES_PER_SECTOR,
                         ZONE_BUF(buffer_idx) + start_sector * BYTES_PER_SECTOR,
                         n_valid_sector * BYTES_PER_SECTOR);

#ifdef DEBUG
                uart_printf("-> start_sector : %d", start_sector);
                uart_printf("-> n_valid_sector : %d, n_invalid_sector : %d, wp_offset : %d, start_sector : %d", n_valid_sector, n_invalid_sector, wp_offset, start_sector);
#endif

                if (n_invalid_sector)
                    mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + (start_sector + n_valid_sector) * BYTES_PER_SECTOR,
                                 0xFFFFFFFF, n_invalid_sector * BYTES_PER_SECTOR);
            }

            flash_finish();

            SETREG(BM_STACK_RDSET, next_read_buf_id); // change bm_read_limit
            SETREG(BM_STACK_RESET, 0x02);             // change bm_read_limit

            g_ftl_read_buf_id = next_read_buf_id;
        }
        else if (lpn < real_end_lpn)
        {

            // read from nand
            // int lpn_in_zone = lpn % (NUM_BANKS * SECTORS_PER_PAGE);
            int lpn_in_zone = lpn % (NUM_BANKS * PAGES_PER_BLK);
            int bank_offset = lpn_in_zone % NUM_BANKS;
            int bank = bank_offset;
            int block = fbg;
            int page = lpn_in_zone / NUM_BANKS;
#ifdef DEBUG
            uart_printf("-> read from nand");
            uart_printf("---> read from fbg: %d bank : %d block :%d page : %d", fbg, bank, block, page);
#endif
            nand_page_ptread_to_host(bank,
                                     block,
                                     page,
                                     start_sector,
                                     n_sector);
        }
        else if (lpn > real_end_lpn)
        {
#ifdef DEBUG
            uart_printf("-> fill with 0xFF");
#endif

            // sata read buffer 조정
            UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

#if OPTION_FTL_TEST == 0
            while (next_read_buf_id == GETREG(SATA_RBUF_PTR))
                ; // wait if the read buffer is full (slow host)
#endif

            // 0xff
            mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + start_sector * BYTES_PER_SECTOR,
                         0xFFFFFFFF, n_sector * BYTES_PER_SECTOR);
            flash_finish();
            SETREG(BM_STACK_RDSET, next_read_buf_id); // change bm_read_limit
            SETREG(BM_STACK_RESET, 0x02);             // change bm_read_limit

            g_ftl_read_buf_id = next_read_buf_id;
        }
        sector_acc += n_sector;
    }

#ifdef DEBUG
    uart_printf("[-] greedy ftl_read");
#endif
}

UINT32 is_bad_block_zone()
{
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++)
    {
        // if (is_bad_block(bank, g_fbg_queue.queue[g_fbg_queue.front])) {
        if (is_bad_block(bank, get_fbg_queue(fbg_queue_front)))
        {
            return 1;
        }
    }

    for (UINT32 list_index = 0; list_index < NUM_RANDOM_BLK + NUM_OP_BLK; list_index++)
    {
        // if (g_random.random_blk_list[list_index] == g_fbg_queue.queue[g_fbg_queue.front])
        if (g_random.random_blk_list[list_index] == get_fbg_queue(fbg_queue_front))
        {
            return 1;
        }
    }

    return 0;
}

UINT32 is_bad_zone(UINT32 vblock)
{
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++)
    {
        if (is_bad_block(bank, vblock))
        {
            return 1;
        }
    }
    return 0;
}

void zns_zone_reset(UINT32 const lba)
{
#ifdef DEBUG
    uart_printf("[+] greedy zns_zone_reset");
#endif

    // get zone_number from SATA buffer
    while (g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR))
        ;
    UINT32 zone_number = read_dram_32(WR_BUF_PTR(g_ftl_write_buf_id) + lba * BYTES_PER_SECTOR);
    g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;
    flash_finish();
    SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
    SETREG(BM_STACK_RESET, 0x01);

#ifdef DEBUG
    uart_printf("-> %u %d", zone_number, get_zone_desc(zone_number, ZD_STATE));
#endif

    if (get_zone_desc(zone_number, ZD_STATE) != ZONE_FULL)
        return;

    // get zone infos
    // struct zone_desc *zone_desc = &zone_descs[zone_number];
    // int fbg = g_z2f[zone_number].fbg;
    int fbg = get_z2f_entry(zone_number, ZE_FBG);
    int vblock = fbg;

    // erase blocks in zone
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++)
        nand_block_erase(bank, vblock);

    // reset target zone
    // zone_desc->wp = zone_desc->slba;
    int slba = get_zone_desc(zone_number, ZD_SLBA);
    set_zone_desc(zone_number, ZD_WP, slba);
    // zone_desc->state = ZONE_EMPTY;
    set_zone_desc(zone_number, ZD_STATE, ZONE_EMPTY);

    // return fbg to queue
    // g_fbg_queue.queue[g_fbg_queue.back] = fbg;
    set_fbg_queue(fbg_queue_back, fbg);
    // g_fbg_queue.back = (g_fbg_queue.back + 1) % MAX_ZONE;
    // fbg_queue_back = (fbg_queue_back + 1) % MAX_ZONE;
    inc_fbg_queue_back();

#ifdef DEBUG
    uart_printf("[-] greedy zns_zone_reset");
#endif
}
void zns_get_desc(UINT32 const lba)
{
#ifdef DEBUG
    // uart_printf("[+] greedy zns_get_desc");
#endif
    // get zone_number, zone_cnt from SATA buffer
    while (g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR))
        ;
    UINT32 zone_number = read_dram_32(WR_BUF_PTR(g_ftl_write_buf_id) + lba * BYTES_PER_SECTOR);
    UINT32 zone_cnt = read_dram_32(WR_BUF_PTR(g_ftl_write_buf_id) + lba * BYTES_PER_SECTOR + sizeof(int));
    g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;
    flash_finish();
    SETREG(BM_STACK_WRSET, g_ftl_write_buf_id);
    SETREG(BM_STACK_RESET, 0x01);

    // print zone metadata
    // struct zone_desc *zone_desc;
    uart_printf("Get Zone Desc(%u, %u): [ ", zone_number, zone_cnt);
    for (int i = 0; i < zone_cnt; i++)
    {
        UINT32 cur_zone = zone_number + i;
        // zone_desc = &zone_descs[cur_zone];
        // uart_printf("%d:%d,%d,%d ", cur_zone, zone_desc->state, zone_desc->slba, zone_desc->wp);
        uart_printf("%d:%d,%d,%d ", cur_zone,
                    // zone_desc->state,
                    get_zone_desc(cur_zone, ZD_STATE),
                    // zone_desc->slba,
                    get_zone_desc(cur_zone, ZD_SLBA),
                    // zone_desc->wp),
                    get_zone_desc(cur_zone, ZD_WP));
    }
    uart_printf("]\n");
#ifdef DEBUG
    // uart_printf("[-] greedy zns_get_desc");
#endif
}

void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
#ifdef DEBUG
    uart_printf("[+] greedy ftl_write");
#endif

    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;

    lpn = lba / SECTORS_PER_PAGE;
    sect_offset = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    // zone reset
    if ((lba == 7) && (num_sectors == 11))
    {
        zns_zone_reset(lba);
        return;
    }
    // zone desc
    else if ((lba == 7) && (num_sectors == 13))
    {
        zns_get_desc(lba);
        return;
    }

    if (is_random_lba(lba))
    {
#ifdef DEBUG
        uart_printf("-> ftl_write to random");
#endif
        while (remain_sects != 0)
        {
            if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
            {
                num_sectors_to_write = remain_sects;
            }
            else
            {
                num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
            }
            // single page write individually
            write_page(lpn, sect_offset, num_sectors_to_write);

            sect_offset = 0;
            remain_sects -= num_sectors_to_write;
            lpn++;
#ifdef DEBUG
            uart_printf("-> g_ftl_write_buf_id: %u", g_ftl_write_buf_id);
#endif
        }
#ifdef DEBUG
        uart_printf("[-] greedy ftl_write");
#endif
        return;
    }

#ifdef DEBUG
    uart_printf("-> ftl_write to sequential");
#endif

    UINT32 zone = lba_to_zone(lba);
#ifdef DEBUG
    uart_printf("-> zone: %d", zone);
#endif

    // start lba not match write pointer or ZONE_FULL
    if ((lba != get_zone_desc(zone, ZD_WP)) || (get_zone_desc(zone, ZD_STATE) == ZONE_FULL))
    {
        g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;
#ifdef DEBUG
        if (lba != get_zone_desc(zone, ZD_WP))
            uart_printf("-> start lba not match write pointer ... failed");
        if (get_zone_desc(zone, ZD_STATE) == ZONE_FULL)
            uart_printf("-> ZONE_FULL ... failed");
#endif
        return;
    }

    if (lba == get_zone_desc(zone, ZD_SLBA))
    {
#ifdef DEBUG
        uart_printf("-> start of zone");
#endif
        // if max zone open
        if (open_zone_count == MAX_OPEN_ZONE)
        {
            g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;
#ifdef DEBUG
            uart_printf("-> max zone open ... failed");
#endif
            return;
        }

        // zone_desc->state = ZONE_OPEN;
        set_zone_desc(zone, ZD_STATE, ZONE_OPEN);
        open_zone_count++;

        // allocate buffer for current zone
        int i;
        for (i = 0; i < MAX_OPEN_ZONE; i++)
        {
            if (b2z[i] == TOTAL_ZONE)
            {
                b2z[i] = zone;
                break;
            }
        }
        ASSERT(i != MAX_OPEN_ZONE);
#ifdef DEBUG
        if (i == MAX_OPEN_ZONE)
            uart_printf("-> [XXXXX] allocate");
#endif

        // zone bad block check
        while (is_bad_block_zone())
        {
            inc_fbg_queue_front();
        }
        int fbg = get_fbg_queue(fbg_queue_front);
        set_z2f_entry(zone, ZE_FBG, fbg);
#ifdef DEBUG
        uart_printf("-> set zone #%d fbg #%d ", zone, fbg);
#endif
        inc_fbg_queue_front();
    }

    UINT32 fbg = get_z2f_entry(zone, ZE_FBG);

    UINT32 end_lba = lba + num_sectors - 1;

    UINT32 start_lpn = lba / SECTORS_PER_PAGE;
    UINT32 end_lpn = end_lba / SECTORS_PER_PAGE;

    UINT32 sector_acc = 0;

    // find buffer for current zone
    UINT32 buffer_idx = TOTAL_ZONE;
    for (int i = 0; i < MAX_OPEN_ZONE; i++)
    {
        if (b2z[i] == zone)
        {
            buffer_idx = i;
            break;
        }
    }
    ASSERT(buffer_idx != TOTAL_ZONE);
#ifdef DEBUG
    if (buffer_idx == TOTAL_ZONE)
        uart_printf("-> [XXXXX] write");
#endif

    for (int lpn = start_lpn; lpn <= end_lpn; lpn++)
    {
        int start_sector = lpn == start_lpn ? lba % SECTORS_PER_PAGE : 0;
        int end_sector = lpn == end_lpn ? end_lba % SECTORS_PER_PAGE : SECTORS_PER_PAGE - 1;
        int n_sector = end_sector - start_sector + 1;

        // write to buffer
        if (end_sector != (SECTORS_PER_PAGE - 1))
        {
#ifdef DEBUG
            uart_printf("-> [%d] write to buffer", lpn);
            uart_printf("---> write to zone buffer #%d sector_acc %d n_sector %d ", buffer_idx, sector_acc, n_sector);
            uart_printf("---> %u %u ", g_ftl_write_buf_id, GETREG(SATA_WBUF_PTR));
#endif

            while (g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR))
                ; // wait if the read buffer is full (slow host)
#ifdef DEBUG
            uart_printf("-> mem_copy");
#endif

            mem_copy(ZONE_BUF(buffer_idx) + start_sector * BYTES_PER_SECTOR,
                     WR_BUF_PTR(g_ftl_write_buf_id) + start_sector * BYTES_PER_SECTOR,
                     n_sector * BYTES_PER_SECTOR);

            g_ftl_write_buf_id = (g_ftl_write_buf_id + 1) % NUM_WR_BUFFERS;

            flash_finish();

            SETREG(BM_STACK_WRSET, g_ftl_write_buf_id); // change bm_write_limit
            SETREG(BM_STACK_RESET, 0x01);               // change bm_write_limit
        }
        // write to nand
        else
        {
            int lpn_in_zone = lpn % (NUM_BANKS * PAGES_PER_BLK);

            int bank_offset = lpn_in_zone % NUM_BANKS;
            int bank = bank_offset;

            int block = fbg;

            int page = lpn_in_zone / NUM_BANKS;

            while (g_ftl_write_buf_id == GETREG(SATA_WBUF_PTR))
                ; // wait if the read buffer is full (slow host)

            // write directly to nand
            if (start_sector == 0)
            {
#ifdef DEBUG
                uart_printf("-> [%d] write directly to nand", lpn);
                uart_printf("---> bank : %d, block : %d, page : %d ", bank, block, page);
#endif
                /* nand_write(bank, block, page, data + sector_acc, &spare); */
                nand_page_ptprogram_from_host(bank,
                                              block,
                                              page,
                                              start_sector,
                                              SECTORS_PER_PAGE);
            }
            // copy to buffer write to nand
            else
            {
#ifdef DEBUG
                uart_printf("-> [%d] copy to buffer and write to nand", lpn);
                uart_printf("---> bank : %d, block : %d, page : %d ", bank, block, page);
#endif
                int wp_offset = get_zone_desc(zone, ZD_WP) % SECTORS_PER_PAGE;
                mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                         ZONE_BUF(buffer_idx),
                         wp_offset * BYTES_PER_SECTOR);

                /* nand_write(bank, block, page, zone_buffers[zone].buffer, &spare); */
                nand_page_ptprogram_from_host(bank,
                                              block,
                                              page,
                                              start_sector,
                                              SECTORS_PER_PAGE);
            }
        }
        sector_acc += n_sector;
        set_zone_desc(zone, ZD_WP, get_zone_desc(zone, ZD_WP) + n_sector);

        // if zone is full
        if (get_zone_desc(zone, ZD_WP) == (get_zone_desc(zone, ZD_SLBA) + ZONE_SIZE))
        {
#ifdef DEBUG
            uart_printf("-> ZONE #%d Full", zone);
#endif
            // zone_desc->state = ZONE_FULL;
            set_zone_desc(zone, ZD_STATE, ZONE_FULL);
            open_zone_count--;

            // buffer 반납
            for (int i = 0; i < MAX_OPEN_ZONE; i++)
            {
                if (b2z[i] == zone)
                {
                    b2z[i] = TOTAL_ZONE;
                    break;
                }
            }
        }

#ifdef DEBUG
        uart_printf("-> g_ftl_write_buf_id: %u", g_ftl_write_buf_id);
#endif
    }

#ifdef DEBUG
    uart_printf("[-] greedy ftl_write");
#endif
}
static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
    CHECK_LPAGE(lpn);
    ASSERT(sect_offset < SECTORS_PER_PAGE);
    ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

    UINT32 bank, old_vpn, new_vpn;
    UINT32 vblock, page_num, page_offset, column_cnt;

    bank = get_num_bank(lpn); // page striping
    page_offset = sect_offset;
    column_cnt = num_sectors;

    new_vpn = assign_new_write_vpn(bank);
    old_vpn = get_vpn(lpn);

    CHECK_VPAGE(old_vpn);
    CHECK_VPAGE(new_vpn);
    ASSERT(old_vpn != new_vpn);

    g_ftl_statistics[bank].page_wcount++;

    // if old data already exist,
    if (old_vpn != NULL)
    {
        vblock = old_vpn / PAGES_PER_BLK;
        page_num = old_vpn % PAGES_PER_BLK;

        //--------------------------------------------------------------------------------------
        // `Partial programming'
        // we could not determine whether the new data is loaded in the SATA write buffer.
        // Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
        // And then, program whole valid data
        //--------------------------------------------------------------------------------------
        if (num_sectors != SECTORS_PER_PAGE)
        {
            // Performance optimization (but, not proved)
            // To reduce flash memory access, valid hole copy into SATA write buffer after reading whole page
            // Thus, in this case, we need just one full page read + one or two mem_copy
            if ((num_sectors <= 8) && (page_offset != 0))
            {
                // one page async read
                nand_page_read(bank,
                               vblock,
                               page_num,
                               FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                             FTL_BUF(bank) + rhole_base,
                             BYTES_PER_PAGE - rhole_base);
                }
            }
            // left/right hole async read operation (two partial page read)
            else
            {
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + column_cnt,
                                     SECTORS_PER_PAGE - (page_offset + column_cnt),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
            }
        }
        // full page write
        page_offset = 0;
        column_cnt = SECTORS_PER_PAGE;
        // invalid old page (decrease vcount)
        set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
    }
    vblock = new_vpn / PAGES_PER_BLK;
    page_num = new_vpn % PAGES_PER_BLK;
    ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

    // write new data (make sure that the new data is ready in the write buffer frame)
    // (c.f FO_B_SATA_W flag in flash.h)
    nand_page_ptprogram_from_host(bank,
                                  vblock,
                                  page_num,
                                  page_offset,
                                  column_cnt);
    // update metadata
    set_lpn(bank, page_num, lpn);
    set_vpn(lpn, new_vpn);
    set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
}
// get vpn from PAGE_MAP
static UINT32 get_vpn(UINT32 const lpn)
{
    CHECK_LPAGE(lpn);
    return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
}
// set vpn to PAGE_MAP
static void set_vpn(UINT32 const lpn, UINT32 const vpn)
{
    CHECK_LPAGE(lpn);
    ASSERT(vpn >= (META_BLKS_PER_BANK * PAGES_PER_BLK) && vpn < (VBLKS_PER_BANK * PAGES_PER_BLK));

    write_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32), vpn);
}
// get valid page count of vblock
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock)
{
    UINT32 vcount;

    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));

    vcount = read_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

    return vcount;
}
// set valid page count of vblock
static void set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount)
{
    ASSERT(bank < NUM_BANKS);
    ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));
    ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

    write_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)), vcount);
}

static UINT32 assign_new_write_vpn(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 write_vpn;
    UINT32 vblock;

    write_vpn = get_cur_write_vpn(bank);
    vblock = write_vpn / PAGES_PER_BLK;

    // NOTE: if next new write page's offset is
    // the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
    if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 2))
    {
        // then, because of the flash controller limitation
        // (prohibit accessing a spare area (i.e. OOB)),
        // thus, we persistenly write a lpn list into last page of vblock.
        mem_copy(FTL_BUF(bank), g_misc_meta[bank].lpn_list_of_cur_vblock, sizeof(UINT32) * PAGES_PER_BLK);
        // fix minor bug
        nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
                            ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR), FTL_BUF(bank));

        mem_set_sram(g_misc_meta[bank].lpn_list_of_cur_vblock, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);

        inc_full_blk_cnt(bank);

        // do garbage collection if necessary
        if (is_full_all_blks(bank))
        {
            garbage_collection(bank);
            return get_cur_write_vpn(bank);
        }

        if (g_misc_meta[bank].num_random_blk < g_random.max_num_random_blk)
        {
            // 있던 zone
            vblock = g_random.random_blk_list[g_misc_meta[bank].num_random_blk++];
        }
        else
        {
            // 새로운 zone (num_random_blk == max 일때)
            // zone bad block check
            while (is_bad_block_zone())
            {
                // g_fbg_queue.front = (g_fbg_queue.front + 1) % MAX_ZONE;
                // fbg_queue_front = (fbg_queue_front + 1) % MAX_ZONE;
                inc_fbg_queue_front();
            }
            // int fbg = g_fbg_queue.queue[g_fbg_queue.front];
            int fbg = get_fbg_queue(fbg_queue_front);
            vblock = fbg;
            // g_fbg_queue.front = (g_fbg_queue.front + 1) % MAX_ZONE;
            // fbg_queue_front = (fbg_queue_front + 1) % MAX_ZONE;
            inc_fbg_queue_front();

            g_random.random_blk_list[g_misc_meta[bank].num_random_blk++] = vblock;
            g_random.max_num_random_blk++;
        }
        ASSERT(vblock != VBLKS_PER_BANK);

        /*         do
                {
                    vblock++;

                    ASSERT(vblock != VBLKS_PER_BANK);
                }while (get_vcount(bank, vblock) == VC_MAX); */
    }
    // write page -> next block
    if (vblock != (write_vpn / PAGES_PER_BLK))
    {
        write_vpn = vblock * PAGES_PER_BLK;
    }
    else
    {
        write_vpn++;
    }
    set_new_write_vpn(bank, write_vpn);

    return write_vpn;
}
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank * (VBLKS_PER_BANK / 8 + 1), vblk_offset) == FALSE)
    {
        return FALSE;
    }
    return TRUE;
}
//------------------------------------------------------------
// if all blocks except one free block are full,
// do garbage collection for making at least one free page
//-------------------------------------------------------------
static void garbage_collection(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);
    g_ftl_statistics[bank].gc_cnt++;

    UINT32 src_lpn;
    UINT32 vt_vblock;
    UINT32 free_vpn;
    UINT32 vcount; // valid page count in victim block
    UINT32 src_page;
    UINT32 gc_vblock;

    g_ftl_statistics[bank].gc_cnt++;

    vt_vblock = get_vt_vblock(bank); // get victim block
    vcount = get_vcount(bank, vt_vblock);
    gc_vblock = get_gc_vblock(bank);
    free_vpn = gc_vblock * PAGES_PER_BLK;

    /*     uart_printf("garbage_collection bank %d, vblock %d",bank, vt_vblock); */

    ASSERT(vt_vblock != gc_vblock);
    ASSERT(vt_vblock >= META_BLKS_PER_BANK && vt_vblock < VBLKS_PER_BANK);
    ASSERT(vcount < (PAGES_PER_BLK - 1));
    ASSERT(get_vcount(bank, gc_vblock) == VC_MAX);
    ASSERT(!is_bad_block(bank, gc_vblock));

    // 1. load p2l list from last page offset of victim block (4B x PAGES_PER_BLK)
    // fix minor bug
    nand_page_ptread(bank, vt_vblock, PAGES_PER_BLK - 1, 0,
                     ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR), FTL_BUF(bank), RETURN_WHEN_DONE);
    mem_copy(g_misc_meta[bank].lpn_list_of_cur_vblock, FTL_BUF(bank), sizeof(UINT32) * PAGES_PER_BLK);
    // 2. copy-back all valid pages to free space
    for (src_page = 0; src_page < (PAGES_PER_BLK - 1); src_page++)
    {
        // get lpn of victim block from a read lpn list
        src_lpn = get_lpn(bank, src_page);
        CHECK_VPAGE(get_vpn(src_lpn));

        // determine whether the page is valid or not
        if (get_vpn(src_lpn) !=
            ((vt_vblock * PAGES_PER_BLK) + src_page))
        {
            // invalid page
            continue;
        }
        ASSERT(get_lpn(bank, src_page) != INVALID);
        CHECK_LPAGE(src_lpn);
        // if the page is valid,
        // then do copy-back op. to free space
        nand_page_copyback(bank,
                           vt_vblock,
                           src_page,
                           free_vpn / PAGES_PER_BLK,
                           free_vpn % PAGES_PER_BLK);
        ASSERT((free_vpn / PAGES_PER_BLK) == gc_vblock);
        // update metadata
        set_vpn(src_lpn, free_vpn);
        set_lpn(bank, (free_vpn % PAGES_PER_BLK), src_lpn);

        free_vpn++;
    }
#if OPTION_ENABLE_ASSERT
    if (vcount == 0)
    {
        ASSERT(free_vpn == (gc_vblock * PAGES_PER_BLK));
    }
#endif
    // 3. erase victim block
    nand_block_erase(bank, vt_vblock);
    ASSERT((free_vpn % PAGES_PER_BLK) < (PAGES_PER_BLK - 2));
    ASSERT((free_vpn % PAGES_PER_BLK == vcount));

    /*     uart_printf("gc page count : %d", vcount); */

    // 4. update metadata
    set_vcount(bank, vt_vblock, VC_MAX);

    // fbg_queue.queue[fbg_queue.back] = vt_vblock;
    // fbg_queue.back = (fbg_queue.back + 1) % MAX_ZONE;
    set_fbg_queue(fbg_queue_back, vt_vblock);
    // fbg_queue_back = (fbg_queue_back + 1) % MAX_ZONE;
    inc_fbg_queue_back();

    set_vcount(bank, gc_vblock, vcount);
    set_new_write_vpn(bank, free_vpn); // set a free page for new write
    set_gc_vblock(bank, vt_vblock);    // next free block (reserve for GC)
    dec_full_blk_cnt(bank);            // decrease full block count
    /* uart_print("garbage_collection end"); */
}
//-------------------------------------------------------------
// Victim selection policy: Greedy
//
// Select the block which contain minumum valid pages
//-------------------------------------------------------------
static UINT32 get_vt_vblock(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 vblock;

    // search the block which has mininum valid pages
    vblock = mem_search_min_max(VCOUNT_ADDR + (bank * VBLKS_PER_BANK * sizeof(UINT16)),
                                sizeof(UINT16),
                                VBLKS_PER_BANK,
                                MU_CMD_SEARCH_MIN_DRAM);

    ASSERT(is_bad_block(bank, vblock) == FALSE);
    ASSERT(vblock >= META_BLKS_PER_BANK && vblock < VBLKS_PER_BANK);
    ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

    return vblock;
}
static void format(void)
{
    UINT32 bank, vblock, vcount_val;

    ASSERT(NUM_MISC_META_SECT > 0);
    ASSERT(NUM_VCOUNT_SECT > 0);

    uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

    uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
    uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
    uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);

    //----------------------------------------
    // initialize DRAM metadata
    //----------------------------------------
    mem_set_dram(PAGE_MAP_ADDR, NULL, PAGE_MAP_BYTES);
    mem_set_dram(VCOUNT_ADDR, NULL, VCOUNT_BYTES);

#ifdef DEBUG
    // uart_printf("[+] zns dram init");
#endif
    // zns init
    for (UINT32 zone_i = 0; zone_i < MAX_ZONE; zone_i++)
    {
        set_zone_desc(zone_i, ZD_STATE, ZONE_EMPTY);
        set_zone_desc(zone_i, ZD_SLBA, zone_i * ZONE_SIZE);
        set_zone_desc(zone_i, ZD_WP, zone_i * ZONE_SIZE);
    }

    for (UINT32 zone_i = 0; zone_i < TOTAL_ZONE; zone_i++)
    {
        set_fbg_queue(zone_i, zone_i);
    }
    fbg_queue_front = 0;
    fbg_queue_back = 0;
#ifdef DEBUG
    // uart_printf("[-] zns dram init");
#endif

    //----------------------------------------
    // erase all blocks except vblock #0
    //----------------------------------------
    for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
    {
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            vcount_val = VC_MAX;
            if (is_bad_block(bank, vblock) == FALSE)
            {
                nand_block_erase(bank, vblock);
                vcount_val = 0;
            }
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16),
                          vcount_val);
        }
    }
    //----------------------------------------
    // initialize SRAM metadata
    //----------------------------------------
    init_metadata_sram();

    // flush metadata to NAND
    logging_pmap_table();
    logging_misc_metadata();

    write_format_mark();
    led(1);
    uart_print("format complete");
}
static void init_metadata_sram(void)
{
    UINT32 bank;
    UINT32 vblock;
    UINT32 mapblk_lbn;

    //----------------------------------------
    // initialize misc. metadata
    //----------------------------------------

    UINT32 set_once = 0;
    max_vblock_zone = 0;

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        /* g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
        g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank); */
        g_misc_meta[bank].free_blk_cnt = NUM_RANDOM_BLK + NUM_OP_BLK;

        // NOTE: vblock #0,1 don't use for user space
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);

        //----------------------------------------
        // assign misc. block
        //----------------------------------------
        // assumption: vblock #1 = fixed location.
        // Thus if vblock #1 is a bad block, it should be allocate another block.
        set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK - 1);
        ASSERT(is_bad_block(bank, MISCBLK_VBN) == FALSE);

        vblock = MISCBLK_VBN;

        //----------------------------------------
        // assign map block
        //----------------------------------------
        mapblk_lbn = 0;
        while (mapblk_lbn < MAPBLKS_PER_BANK)
        {
            vblock++;
            ASSERT(vblock < VBLKS_PER_BANK);
            if (is_bad_block(bank, vblock) == FALSE)
            {
                set_mapblk_vpn(bank, mapblk_lbn, vblock * PAGES_PER_BLK);
                write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
                mapblk_lbn++;
            }
        }
        if (vblock > max_vblock_zone)
        {
            max_vblock_zone = vblock;
        }

        g_misc_meta[bank].num_random_blk = 0;
    }

    // zns init
    for (UINT32 zone_i = 0; zone_i < MAX_ZONE; zone_i++)
    {
        // zone_descs[zone_i].state = ZONE_EMPTY;
        // zone_descs[zone_i].slba = zone_i * ZONE_SIZE;
        // zone_descs[zone_i].wp = zone_descs[zone_i].slba;
        set_zone_desc(zone_i, ZD_STATE, ZONE_EMPTY);
        set_zone_desc(zone_i, ZD_SLBA, zone_i * ZONE_SIZE);
        set_zone_desc(zone_i, ZD_WP, zone_i * ZONE_SIZE);

        // g_z2f[zone_i].fbg = -1;
        set_z2f_entry(zone_i, ZE_FBG, -1);
    }

    for (UINT32 zone_i = 0; zone_i < TOTAL_ZONE; zone_i++)
    {
        // g_fbg_queue.queue[zone_i] = zone_i;
        set_fbg_queue(zone_i, zone_i);
    }
    // g_fbg_queue.front = max_vblock_zone;
    // g_fbg_queue.back = max_vblock_zone;
    fbg_queue_front = max_vblock_zone;
    fbg_queue_back = max_vblock_zone;

    for (UINT32 list_index = 0; list_index < NUM_RANDOM_BLK + NUM_OP_BLK; list_index++)
    {
        g_random.random_blk_list[list_index] = TOTAL_ZONE;
    }

    //----------------------------------------
    // assign free block for gc
    //----------------------------------------
    vblock = max_vblock_zone;
    do
    {
        vblock++;
        // set free block
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            // NOTE: free block should not be secleted as a victim @ first GC
            write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
            set_gc_vblock(bank, vblock);

            g_misc_meta[bank].num_random_blk = 1;
        }
        g_random.max_num_random_blk = 0;
        g_random.random_blk_list[g_random.max_num_random_blk] = vblock;
        g_random.max_num_random_blk++;

        // queue에서 op block은 빼야함?

        ASSERT(vblock < VBLKS_PER_BANK);
    } while (is_bad_zone(vblock) == TRUE);
    //}while(is_bad_block(bank, vblock) == TRUE);

    //----------------------------------------
    // assign free vpn for first new write
    //----------------------------------------
    do
    {
        vblock++;
        // 현재 next vblock부터 새로운 데이터 저장 시작
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            set_new_write_vpn(bank, vblock * PAGES_PER_BLK);
        }
        ASSERT(vblock < VBLKS_PER_BANK);
    } while (is_bad_zone(vblock) == TRUE);

    // g_fbg_queue.front = vblock;
    // g_fbg_queue.front = (g_fbg_queue.front + 1) % MAX_ZONE;
    fbg_queue_front = vblock;
    // fbg_queue_front = (fbg_queue_front + 1) % MAX_ZONE;
    inc_fbg_queue_front();
}
// logging misc + vcount metadata
static void logging_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_addr = VCOUNT_ADDR;
    UINT32 vcount_bytes = NUM_VCOUNT_SECT * BYTES_PER_SECTOR; // per bank
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;      // entire vcount data
    UINT32 bank;

    flash_finish();

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        inc_miscblk_vpn(bank);

        // note: if misc. meta block is full, just erase old block & write offset #0
        if ((get_miscblk_vpn(bank) / PAGES_PER_BLK) != MISCBLK_VBN)
        {
            nand_block_erase(bank, MISCBLK_VBN);
            set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK); // vpn = 128
        }
        // copy misc. metadata to FTL buffer
        mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

        // copy vcount metadata to FTL buffer
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(FTL_BUF(bank) + misc_meta_bytes, vcount_addr, vcount_bytes);
            vcount_addr += vcount_bytes;
        }
    }
    // logging the misc. metadata to nand flash
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        nand_page_ptprogram(bank,
                            get_miscblk_vpn(bank) / PAGES_PER_BLK,
                            get_miscblk_vpn(bank) % PAGES_PER_BLK,
                            0,
                            NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                            FTL_BUF(bank));
    }
    flash_finish();
}
static void logging_pmap_table(void)
{
    UINT32 pmap_addr = PAGE_MAP_ADDR;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 mapblk_vpn;
    UINT32 bank;
    UINT32 pmap_boundary = PAGE_MAP_ADDR + PAGE_MAP_BYTES;
    BOOL32 finished = FALSE;

    for (UINT32 mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            inc_mapblk_vpn(bank, mapblk_lbn);

            mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);

            // note: if there is no free page, then erase old map block first.
            if ((mapblk_vpn % PAGES_PER_BLK) == 0)
            {
                // erase full map block
                nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

                // next vpn of mapblk is offset #0
                set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);
                mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
            }
            // copy the page mapping table to FTL buffer
            mem_copy(FTL_BUF(bank), pmap_addr, pmap_bytes);

            // logging update page mapping table into map_block
            nand_page_ptprogram(bank,
                                mapblk_vpn / PAGES_PER_BLK,
                                mapblk_vpn % PAGES_PER_BLK,
                                0,
                                pmap_bytes / BYTES_PER_SECTOR,
                                FTL_BUF(bank));
            pmap_addr += pmap_bytes;
        }
        if (finished)
        {
            break;
        }
    }
    flash_finish();
}
// load flushed FTL metadta
static void load_metadata(void)
{
    load_misc_metadata();
    load_pmap_table();
}
// misc + VCOUNT
static void load_misc_metadata(void)
{
    UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_bytes = NUM_VCOUNT_SECT * BYTES_PER_SECTOR;
    UINT32 vcount_addr = VCOUNT_ADDR;
    UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;

    UINT32 load_flag = 0;
    UINT32 bank, page_num;
    UINT32 load_cnt = 0;

    flash_finish();

    disable_irq();
    flash_clear_irq(); // clear any flash interrupt flags that might have been set

    // scan valid metadata in descending order from last page offset
    for (page_num = PAGES_PER_BLK - 1; page_num != ((UINT32)-1); page_num--)
    {
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (load_flag & (0x1 << bank))
            {
                continue;
            }
            // read valid metadata from misc. metadata area
            nand_page_ptread(bank,
                             MISCBLK_VBN,
                             page_num,
                             0,
                             NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
        }
        flash_finish();

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (!(load_flag & (0x1 << bank)) && !(BSP_INTR(bank) & FIRQ_ALL_FF))
            {
                load_flag = load_flag | (0x1 << bank);
                load_cnt++;
            }
            CLR_BSP_INTR(bank, 0xFF);
        }
    }
    ASSERT(load_cnt == NUM_BANKS);

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        // misc. metadata
        mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));

        // vcount metadata
        if (vcount_addr <= vcount_boundary)
        {
            mem_copy(vcount_addr, FTL_BUF(bank) + misc_meta_bytes, vcount_bytes);
            vcount_addr += vcount_bytes;
        }
    }
    enable_irq();
}
static void load_pmap_table(void)
{
    UINT32 pmap_addr = PAGE_MAP_ADDR;
    UINT32 temp_page_addr;
    UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
    UINT32 pmap_boundary = PAGE_MAP_ADDR + (NUM_LPAGES * sizeof(UINT32));
    UINT32 mapblk_lbn, bank;
    BOOL32 finished = FALSE;

    flash_finish();

    for (mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
    {
        temp_page_addr = pmap_addr; // backup page mapping addr

        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (finished)
            {
                break;
            }
            else if (pmap_addr >= pmap_boundary)
            {
                finished = TRUE;
                break;
            }
            else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                finished = TRUE;
                pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // read page mapping table from map_block
            nand_page_ptread(bank,
                             get_mapblk_vpn(bank, mapblk_lbn) / PAGES_PER_BLK,
                             get_mapblk_vpn(bank, mapblk_lbn) % PAGES_PER_BLK,
                             0,
                             pmap_bytes / BYTES_PER_SECTOR,
                             FTL_BUF(bank),
                             RETURN_ON_ISSUE);
            pmap_addr += pmap_bytes;
        }
        flash_finish();

        pmap_bytes = BYTES_PER_PAGE;
        for (bank = 0; bank < NUM_BANKS; bank++)
        {
            if (temp_page_addr >= pmap_boundary)
            {
                break;
            }
            else if (temp_page_addr + BYTES_PER_PAGE >= pmap_boundary)
            {
                pmap_bytes = (pmap_boundary - temp_page_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
            }
            // copy page mapping table to PMAP_ADDR from FTL buffer
            mem_copy(temp_page_addr, FTL_BUF(bank), pmap_bytes);

            temp_page_addr += pmap_bytes;
        }
        if (finished)
        {
            break;
        }
    }
}
static void write_format_mark(void)
{
    // This function writes a format mark to a page at (bank #0, block #0).

#ifdef __GNUC__
    extern UINT32 size_of_firmware_image;
    UINT32 firmware_image_pages = (((UINT32)(&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#else
    extern UINT32 Image$$ER_CODE$$RO$$Length;
    extern UINT32 Image$$ER_RW$$RW$$Length;
    UINT32 firmware_image_bytes = ((UINT32)&Image$$ER_CODE$$RO$$Length) + ((UINT32)&Image$$ER_RW$$RW$$Length);
    UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#endif

    UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

    mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

    SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
    SETREG(FCP_BANK, REAL_BANK(0));
    SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
    SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); // DRAM -> flash
    SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
    SETREG(FCP_COL, 0);
    SETREG(FCP_ROW_L(0), format_mark_page_offset);
    SETREG(FCP_ROW_H(0), format_mark_page_offset);

    // At this point, we do not have to check Waiting Room status before issuing a command,
    // because we have waited for all the banks to become idle before returning from format().
    SETREG(FCP_ISSUE, NULL);

    // wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
    while ((GETREG(WR_STAT) & 0x00000001) != 0)
        ;

    // wait until bank #0 finishes the write operation
    while (BSP_FSM(0) != BANK_IDLE)
        ;
}
static BOOL32 check_format_mark(void)
{
    // This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

#ifdef __GNUC__
    extern UINT32 size_of_firmware_image;
    UINT32 firmware_image_pages = (((UINT32)(&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#else
    extern UINT32 Image$$ER_CODE$$RO$$Length;
    extern UINT32 Image$$ER_RW$$RW$$Length;
    UINT32 firmware_image_bytes = ((UINT32)&Image$$ER_CODE$$RO$$Length) + ((UINT32)&Image$$ER_RW$$RW$$Length);
    UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#endif

    UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
    UINT32 temp;

    flash_clear_irq(); // clear any flash interrupt flags that might have been set

    SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
    SETREG(FCP_BANK, REAL_BANK(0));
    SETREG(FCP_OPTION, FO_E);
    SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); // flash -> DRAM
    SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
    SETREG(FCP_COL, 0);
    SETREG(FCP_ROW_L(0), format_mark_page_offset);
    SETREG(FCP_ROW_H(0), format_mark_page_offset);

    // At this point, we do not have to check Waiting Room status before issuing a command,
    // because scan list loading has been completed just before this function is called.
    SETREG(FCP_ISSUE, NULL);

    // wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
    while ((GETREG(WR_STAT) & 0x00000001) != 0)
        ;

    // wait until bank #0 finishes the read operation
    while (BSP_FSM(0) != BANK_IDLE)
        ;

    // Now that the read operation is complete, we can check interrupt flags.
    temp = BSP_INTR(0) & FIRQ_ALL_FF;

    // clear interrupt flags
    CLR_BSP_INTR(0, 0xFF);

    if (temp != 0)
    {
        return FALSE; // the page contains all-0xFF (the format mark does not exist.)
    }
    else
    {
        return TRUE; // the page contains something other than 0xFF (it must be the format mark)
    }
}

// BSP interrupt service routine
void ftl_isr(void)
{
    UINT32 bank;
    UINT32 bsp_intr_flag;

    uart_print("BSP interrupt occured...");
    // interrupt pending clear (ICU)
    SETREG(APB_INT_STS, INTR_FLASH);

    for (bank = 0; bank < NUM_BANKS; bank++)
    {
        while (BSP_FSM(bank) != BANK_IDLE)
            ;
        // get interrupt flag from BSP
        bsp_intr_flag = BSP_INTR(bank);

        if (bsp_intr_flag == 0)
        {
            continue;
        }
        UINT32 fc = GETREG(BSP_CMD(bank));
        // BSP clear
        CLR_BSP_INTR(bank, bsp_intr_flag);

        // interrupt handling
        if (bsp_intr_flag & FIRQ_DATA_CORRUPT)
        {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
            uart_print("FIRQ_DATA_CORRUPT occured...");
        }
        if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L))
        {
            uart_printf("BSP interrupt at bank: 0x%x", bank);
            if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG)
            {
                uart_print("find runtime bad block when block program...");
            }
            else
            {
                ASSERT(fc == FC_ERASE);
            }
        }
    }
}
