  
  
   THIS IS THE README FILE FOR THE PROTECTED MODE SCHEME 4.0 RELEASE

  This readme file provides the instructions for a build of the Protected
  Mode Scheme Diskette by itself. It may also be built as a part of the
  normal PC Scheme build procedure as described in the file README.
    
  A. Materials to be provided by Software Control:
  
     1.) One blank, formatted, double-sided, double-density  (360KB)
         floppy diskettes.  They should already be formatted.
  
     2.) Business Pro with:
         -  640K memory
	 -  one high-density (1.2 MByte) floppy drive (drive A)
	 -  one low-density (360 KByte) drive (drive B)
	 -  a printer
	 -  one Winchester hard disk drive with at least 10 Mbytes free
  
     3.) MS-DOS Operating System diskette vers. 3.21 (P/N 2538155-1610
         AND 1611)
  
     4.) MACRO ASSEMBLER version 4.00 diskette (P/N 2546114).
  
     5.) LATTICE 'C' COMPILER version 3.05  diskettes (P/N 2249759).
  
     6.) Dater diskette version 1.20 (P/N 2223081-1610).
  
  
  
  B. Materials to be provided by the Scheme Development group:
  
     1.) TI PC SCHEME SOURCE diskette #1 (P/N 2537903-2620).
  
     2.) TI PC SCHEME SOURCE diskette #2 (P/N 2537903-2621).
  
     3.) TI PC SCHEME SOURCE diskette #3 (P/N 2537903-2622).
  
     4.) TI PC SCHEME SOURCE diskette #4 (P/N 2537903-2623).
  
  
  C. Release procedure steps:
  
     1.) Boot the PC from MS-DOS diskette.
  
     2.) Enter the date and time when you are requested to do so.
  
     3.) Format the Winchester as follows:
  
	    - Type FORMAT E: /S (and pressing RETURN).
	    - Respond to the prompt for drive type with appropriate number.
  
     4.) Copy all the files on the MS-DOS diskette onto the Winchester by
	 typing:
  
	       COPY *.* E:/V (and pressing RETURN)
  
     4.1) Modify the CONFIG.SYS file to include at least 15 files:
  
	       FILES=20
	       BUFFERS=15
  
     5.) Reboot the system from the Winchester.
  
     6.) Enter the date and time when you are requested to do so.
  
     7.) Remove the MS-DOS diskette from drive A: and insert the PC SCHEME
	 SOURCE diskette #1 (P/N 2537903-2620) in drive A:
  
     8.) Begin execution of the batch stream to build PC SCHEME:
  
	    - Press the PRNT key (to cause subsequent messages to be echoed
				  to the printer).
  
	    - Type A:MASTER protected
  
	 NOTE: protected must be in lower case!!!

	 The batch stream will instruct you to insert the diskettes listed
	 above.  Once the necessary files have been copied to the Winchester,
	 the actual build process will begin and no further attention is
	 required until the installation disks are ready to be made
	 (the batch stream will wait for you at that point).
  
     9.) Each of the assemblies in the batch stream should terminate with the
	 following message:
  
	   Warning Severe
	   Errors  Errors
	   0	   0
  
    10.) C compilations may produce warning messages and this is all right.
	 There should be no error messages, however.
  
    11.) DOS messages about "unable to create directory" during CD commands
         or "file not found" during DEL commands can be ignored.
  
    12.) When you are prompted to, insert a blank formatted 360KB diskette
         into drive B (*not* drive A) and press RETURN to continue.
  
    13.) Label the diskette as:
  
		PC SCHEME PROTECTED MODE DISKETTE,
		(P/N 2537903-1615).
  
    14.) You're done.
