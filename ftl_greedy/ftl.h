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
// GreedyFTL header file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//

#ifndef FTL_H
#define FTL_H


/////////////////
// DRAM buffers
/////////////////

#define NUM_RW_BUFFERS		((DRAM_SIZE - DRAM_BYTES_OTHER) / BYTES_PER_PAGE - 1)
#define NUM_RD_BUFFERS		(((NUM_RW_BUFFERS / 8) + NUM_BANKS - 1) / NUM_BANKS * NUM_BANKS)
#define NUM_WR_BUFFERS		(NUM_RW_BUFFERS - NUM_RD_BUFFERS)
#define NUM_COPY_BUFFERS	NUM_BANKS_MAX
#define NUM_FTL_BUFFERS		NUM_BANKS
#define NUM_HIL_BUFFERS		1
#define NUM_TEMP_BUFFERS	1

// #define NUM_ZONE_BUFFERS    MAX_ZONE// zone buffer
#define NUM_ZONE_BUFFERS    MAX_OPEN_ZONE// zone buffer
#define NUM_P_RAND_ZONE       7 // 6 blk + 1 op blk
#define NUM_L_RAND_ZONE       6
#define NUM_SEQ_ZONE          TOTAL_ZONE - NUM_L_RAND_ZONE

#define DRAM_BYTES_OTHER	((NUM_COPY_BUFFERS + NUM_FTL_BUFFERS + NUM_HIL_BUFFERS + NUM_TEMP_BUFFERS + NUM_ZONE_BUFFERS) * BYTES_PER_PAGE \
+ BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES + ZONE_BUF_BYTES + Z2F_ENTRY_BYTES + ZONE_DESC_BYTES+ FBG_QUEUE_BYTES)

#define WR_BUF_PTR(BUF_ID)	(WR_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define WR_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - WR_BUF_ADDR) / BYTES_PER_PAGE)
#define RD_BUF_PTR(BUF_ID)	(RD_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define RD_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - RD_BUF_ADDR) / BYTES_PER_PAGE)

#define _COPY_BUF(RBANK)	(COPY_BUF_ADDR + (RBANK) * BYTES_PER_PAGE)
#define COPY_BUF(BANK)		_COPY_BUF(REAL_BANK(BANK))
#define FTL_BUF(BANK)       (FTL_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

#define ZONE_BUF(ZONE)      (ZONE_BUF_ADDR + ((ZONE) * BYTES_PER_PAGE))

///////////////
// ZNS
//////////////
#define RANDOM_AREA_ZONE 6 
#define DEG_ZONE NUM_BANKS
#define MAX_OPEN_ZONE 6

#define MAX_ZONE (NUM_BANKS / DEG_ZONE * (VBLKS_PER_BANK - MAX_OPEN_ZONE))
#define ZONE_SIZE (DEG_ZONE * PAGES_PER_VBLK * SECTORS_PER_PAGE) // sectors per zone
#define MAX_LBA (MAX_ZONE * ZONE_SIZE)
#define TOTAL_ZONE ((NUM_BANKS / DEG_ZONE) * VBLKS_PER_BANK)
#define BYTES_PER_ZONE  ZONE_SIZE * BYTES_PER_SECTOR// bytes per zone

#define NUM_RANDOM_BLK 6
#define NUM_OP_BLK 1

static inline int lba_to_zone(int lba) { return lba / ZONE_SIZE; }

typedef enum
{
  ZONE_EMPTY = 0,
  ZONE_OPEN = 1,
  ZONE_FULL = 2,
  ZONE_TLOPEN = 3,
} zone_state;

struct zone_desc
{
  int state;
  int slba;
  int wp;
};
typedef enum
{
  ZD_STATE = 0,
  ZD_SLBA = 1,
  ZD_WP = 2,
} zone_desc_elem;

struct z2f_entry
{
  int fbg;
  /*   int log_fbg;
    int log_wp;
    UINT32 valid_arr[ZONE_SIZE]; */
};
typedef enum
{
  ZE_FBG = 0,
} z2f_entry_elem;

// typedef struct _fbg_queue {
//   int queue[TOTAL_ZONE];
//   int front;
//   int back;
// }fbg_queue;

///////////////////////////////
// DRAM segmentation
///////////////////////////////

#define RD_BUF_ADDR			DRAM_BASE										// base address of SATA read buffers
#define RD_BUF_BYTES		(NUM_RD_BUFFERS * BYTES_PER_PAGE)

#define WR_BUF_ADDR			(RD_BUF_ADDR + RD_BUF_BYTES)					// base address of SATA write buffers
#define WR_BUF_BYTES		(NUM_WR_BUFFERS * BYTES_PER_PAGE)

#define COPY_BUF_ADDR		(WR_BUF_ADDR + WR_BUF_BYTES)					// base address of flash copy buffers
#define COPY_BUF_BYTES		(NUM_COPY_BUFFERS * BYTES_PER_PAGE)

#define FTL_BUF_ADDR		(COPY_BUF_ADDR + COPY_BUF_BYTES)				// a buffer dedicated to FTL internal purpose
#define FTL_BUF_BYTES		(NUM_FTL_BUFFERS * BYTES_PER_PAGE)

#define HIL_BUF_ADDR		(FTL_BUF_ADDR + FTL_BUF_BYTES)					// a buffer dedicated to HIL internal purpose
#define HIL_BUF_BYTES		(NUM_HIL_BUFFERS * BYTES_PER_PAGE)

#define TEMP_BUF_ADDR		(HIL_BUF_ADDR + HIL_BUF_BYTES)					// general purpose buffer
#define TEMP_BUF_BYTES		(NUM_TEMP_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(TEMP_BUF_ADDR + TEMP_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

#define PAGE_MAP_ADDR		(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)			// page mapping table
#define PAGE_MAP_BYTES		((NUM_LPAGES * sizeof(UINT32) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR)

#define VCOUNT_ADDR			(PAGE_MAP_ADDR + PAGE_MAP_BYTES)
#define VCOUNT_BYTES		((NUM_BANKS * VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR)

// #define BLKS_PER_BANK		VBLKS_PER_BANK

// zone buffer

#define ZONE_BUF_ADDR		(VCOUNT_ADDR + VCOUNT_BYTES)					
#define ZONE_BUF_BYTES		(NUM_ZONE_BUFFERS * BYTES_PER_PAGE)

#define Z2F_ENTRY_ADDR  (ZONE_BUF_ADDR + ZONE_BUF_BYTES)
#define Z2F_ENTRY_BYTES (MAX_ZONE * sizeof(struct z2f_entry))

#define ZONE_DESC_ADDR  (Z2F_ENTRY_ADDR + Z2F_ENTRY_BYTES)
#define ZONE_DESC_BYTES (MAX_ZONE * sizeof(struct zone_desc))

#define FBG_QUEUE_ADDR  (ZONE_DESC_ADDR + ZONE_DESC_BYTES)
#define FBG_QUEUE_BYTES (sizeof(int) * TOTAL_ZONE)


///////////////////////////////
// FTL public functions
///////////////////////////////

void ftl_open(void);
void ftl_read(UINT32 const lba, UINT32 const num_sectors);
void ftl_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_flush(void);
void ftl_isr(void);

// #define Z2F_ENTRY(ZONE, INDEX)      (Z2F_ENTRY_ADDR + ((ZONE) * sizeof(z2f_entry) + (INDEX) * sizeof(int)))
// #define ZONE_DESC(ZONE, INDEX)      (ZONE_DESC_ADDR + ((ZONE) * sizeof(z2f_entry) + (INDEX) * sizeof(int)))

#endif //FTL_H
