% GE_read:
% Purpose -   Read a Genesis format image data file 
% Usage   -   A = GE_read('filename')
%           where
%             'filename' -- is a string containing filename (full path)
%             The image data is returned in the matrix A
%
        function A = GE_read(fname)
%

genesis_magic = 'IMGF'; 
DEBUG = 0;
   
   fid=fopen(fname,'r', 'b');
   if fid>=0
		magic=fread(fid, 4, 'char');
      %% convert the read bytes into a string
      magic_str = sprintf('%s', magic);
	  	if DEBUG fprintf(1,'Genesis magic number: %s\n', magic_str); end;
	   if strcmp(genesis_magic, magic_str) 
			headerLength=fread(fid, 1, 'int32');
			if DEBUG fprintf(1,'Image Header Size: %d\n',headerLength); end;
			width=fread(fid, 1, 'int32');
			if DEBUG fprintf(1,'Image Width: %d\n',width); end;
			height=fread(fid, 1, 'int32');
			if DEBUG fprintf(1,'Image Height: %d\n',height); end;
			depth=fread(fid, 1, 'int32');
			if DEBUG fprintf(1,'Image bytes per pixel: %d\n',depth); end;
	   else  %% if images are not Genesis , this is a hack for SIGNA and other ushort images
      	fprintf(1, 'This is NOT a GENESIS image\n');
			fseek(fid, 0, 'eof');			
			pos = ftell(fid);
			width = 2^((round(log2(pos))-1)/2);
			height = 2^((round(log2(pos))-1)/2);
			headerLength = pos - width*height*2;
	   end
      
      %% Seek from start of file to end of header
      fseek(fid,headerLength,'bof'); 
      %% Read an array of width*height ushorts
		a=fread(fid,[width height],'ushort');
		m = width;
      n= height;
      fclose(fid);
      %% reformat the read bytes into matrix A
      A=a(1:m,1:n)';
      
   else %% if file open fails
              fprintf(1, '\nfile %s could not be opened\n', fname)
   end;







