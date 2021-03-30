import re, sys, struct
from PIL import Image

class LZDecomp:
	def __init__(self):
		pass

	def rb(self, raw=False):
		if raw:
			b = self.raw[self.ind:self.ind+1]
		else:
			b = self.raw[self.ind]
		self.ind += 1
		return b

	def decomp(self, raw):
		self.raw = raw
		self.ind = 0

		if self.rb() != 0x10:
			return -1 # NOT LZ
		data_length = self.rb() + self.rb() * 0x100 + self.rb() * 0x10000
		datout = b''
		while True:
			bitfield = self.rb()
			for i in range(7,-1,-1):
				if bitfield & (1 << i):
					# compressed
					dt = self.rb()
					l = dt // 16 + 3
					d = (((dt & 0xf) * 0x100) | self.rb()) + 1

					#r5 = self.rb()
					#store = r5
					#r6 = 3
					#r3 = (r5>>4)+r6
					#r6 = store
					#r5 = r6&0xf
					#r12 = r5<<8
					#r6 = self.rb()
					#r5 = r6|r12
					#r12 = r5+1

					#l = r3
					#d = r12
					#dt = store
					for n in range(l):
						try:
							bk = datout[-d]
						except:
							# error
							return -2 # DECOMPRESS ERROR
						datout += struct.pack('B', bk)
						if len(datout) == data_length:
							return datout, self.ind
				else:
					datout += self.rb(1)
					if len(datout) == data_length:
						return datout, self.ind

DEFPAL_GRAYSCALE = tuple([(16*x,16*x,16*x) for x in range(16)])[::-1]

def pal2rgb(raw):
	assert len(raw) == 16 * 2
	cols = []
	for i in range(16):
		cur = struct.unpack('H', raw[i*2:i*2+2])[0]
		r = (cur >>  0) & 0x1f
		g = (cur >>  5) & 0x1f
		b = (cur >> 10) & 0x1f
		cols.append((r*8,g*8,b*8))
	return cols

def dat2png(pic, pal, w=1):
	if isinstance(pal, bytes):
		pal = pal2rgb(pal)
	imgw = w * 8
	imgh = (len(pic) * 2) // imgw
	h = imgh // 8
	img = Image.new('RGB', (imgw, imgh))
	pxl = img.load()
	i = 0
	for y in range(h):
		for x in range(w):
			for yy in range(8):
				for xx in range(4):
					byt = pic[i]
					lo = byt & 0xf
					hi = (byt >> 4) & 0xf
					pxl[x*8+xx*2,  y*8+yy] = pal[lo]
					pxl[x*8+xx*2+1,y*8+yy] = pal[hi]

					i += 1

	return img

def rearrange(img, tw, th, w2=-1,h2=-1):
	if w2 == -1 and h2 == -1:
		raise Exception
	w1 = img.size[0] // tw
	h1 = img.size[1] // th
	if w2 == -1:
		assert h2 != -1
		w2 = (w1 * h1) // h2
	elif h2 == -1:
		assert w2 != -1
		h2 = (w1 * h1) // w2
	assert w2 * h2 == w1 * h1
	imgout = Image.new('RGB', (w2*tw,h2*th))
	for y in range(h1):
		for x in range(w1):
			i = y * w1 + x
			x2 = i % w2
			y2 = i // w2
			rect = (x*tw, y*th, x*tw+tw, y*th+th)
			imgout.paste(img.crop(rect), (x2*tw,y2*th))
	return imgout

s2b = lambda x: struct.pack('B', int(x[2:], 16))
s2h = lambda x: struct.pack('H', int(x[2:], 16))

DEBUG = 0

re1 = re.compile(r'\s*((?:0x[0-9a-fA-F]{2},\s*)+)')
re2 = re.compile(r'\s*((?:0x[0-9a-fA-F]{4},\s*)+)')

re_list = (re1, re2)
def matchall(x):
	m_out = []
	for r in re_list:
		m_out.append(r.match(x))
	return tuple(m_out)

def conv_line(l):
	m = matchall(l)
	if m[0]: # 8-bit
		byts = m[0].groups()[0].split(',')[:-1]
		raw = b''
		for b in byts:
			raw += s2b(b)
		return raw
	elif m[1]: # 16-bit
		byts = m[1].groups()[0].split(',')[:-1]
		raw = b''
		for b in byts:
			raw += s2h(b)
		return raw
	else: # Unknown
		if DEBUG:
			print(l)

class Parser:
	SPECIAL_VARS = ('filelineno','$1')
	ERR_NO_FILE = 'no file loaded'
	ERR_NO_PALETTE = 'no palette loaded'
	ERR_NO_IMAGE = 'no image loaded'
	ERR_UNKNOWN_COMMAND = 'unknown command "{}"'
	ERR_EOF = 'unexpected end of file'
	ERR_FILE_NOT_FOUND = 'file "{}" not found'
	ERR_UNKNOWN_PALETTE = 'unknown palette "{}"'
	ERR_NYI = 'not yet implemented'
	ERR_STACK_DATATYPE_NOT_FOUND = 'data type "{}" not found on stack'
	ERR_VALUE = 'invalid value "{}" for "{}"'
	ERR_LZ_INVALID_DATA = 'could not decompress, not LZ data'
	ERR_LZ_DECOMPRESS = 'could not decompress'
	ERR_FIND = 'could not find label "{}"'
	def __init__(self, fn):
		self.lins = open(fn).read().split('\n')
		self.fil = None
		self.img = None
		self.pal = None
		self.fil_linno = 0
		self.buf_store = b''
		self.errored = False
		self.loop_stack = []
		self.var_dict = {}
		self.scr_lineno = 0
		self.data_stack = []
		self.cur_cmd = None

		self.lz = LZDecomp()

	def err(self, s, a=None):
		self.errored = True
		if a:
			print('ERROR: ' + s.format(*a))
		else:
			print('ERROR: ' + s)

	def parsearg(self, s):
		if s in self.SPECIAL_VARS:
			if s == 'filelineno':
				return self.fil_linno
			elif s == '$1':
				return sys.argv[2]
		if s in self.var_dict.keys():
			return self.var_dict[s]
		return int(s)

	def get_special_var_dict(self):
		d = {}
		d['filelineno'] = self.fil_linno
		for i in range(1, 10):
			if len(sys.argv) >= i + 2:
				d['${}'.format(i)] = sys.argv[i+1]
			else:
				d['${}'.format(i)] = None
		return d

	def parsefn(self, s):
		return s.format(**self.var_dict, **self.get_special_var_dict())

	def run(self):
		scanning = True
		while self.scr_lineno < len(self.lins):
			_l = self.lins[self.scr_lineno]
			if _l == ';;;':
				scanning = not scanning
			if not scanning:
				self.scr_lineno += 1
				continue
			if self.errored:
				print('     : at line {}'.format(self.scr_lineno))
				return
			l = _l.split(';', 1)[0].strip()
			if l == '':
				self.scr_lineno += 1
				continue
			cmd = l.split(' ', 1)
			a = cmd[0]
			if len(cmd) == 1:
				b = None
			else:
				b = cmd[1].split(' ')

			self.cur_cmd = a
			if a == 'open':
				# arg1: filename
				self.open_file(self.parsefn(b[0]))
			elif a == 'setvar':
				# arg1: var type (int, str)
				# arg2: var name
				# arg3: var value
				self.set_var(b[0], b[1], b[2])
			elif a == 'getpalette':
				# arg1: number of palettes
				self.get_palette(self.parsearg(b[0]))
			elif a == 'getimage':
				# arg1: palette index
				# arg2: width (in 8x8 tiles)
				# arg3: byte count (or -1 for rest of file)
				self.get_image(self.parsearg(b[2]), self.parsearg(b[0]), self.parsearg(b[1]))
			elif a == 'getcompressedimage':
				# arg1: palette index
				# arg2: width (in 8x8 tiles)
				self.get_compressed_image(self.parsearg(b[0]), self.parsearg(b[1]))
			elif a == 'save':
				# arg1: filename
				self.save_image(self.parsefn(b[0]))
			elif a == 'for':
				# arg1: var name
				# arg2: range (start,end)
				# N.B. includes start, not end
				self.setup_for(b[0], b[1])
			elif a == 'donefor':
				self.for_done()
			elif a == 'newimage':
				self.img = Image.new('RGB', (0, 0))
			elif a == 'pushimage':
				self.pushdata(self.img, 'image')
			elif a == 'popimage':
				self.img = self.popdata('image')
			elif a == 'popandappendimage':
				# arg1: u, d, l, or r
				curimg = self.img
				self.img = self.popdata('image')
				self.append_image(curimg, b[0])
			elif a == 'pushline':
				self.pushdata(self.fil_linno, 'lineno')
			elif a == 'popline':
				self.fil_linno = self.popdata('lineno')
			elif a == 'arrange':
				# arg1: tile width
				# arg2: tile height
				# arg3: out num tiles x
				# arg4: out num tiles y
				# if arg3 or arg4 is -1, the other will be used
				#   to guess what it should be
				self.arr_image(self.parsearg(b[0]), self.parsearg(b[1]), self.parsearg(b[2]), self.parsearg(b[3]))
			elif a == 'findlabel':
				# arg1: label string
				self.find_label(b[0])
			elif a == 'setpalette':
				# arg1: palette name
				# OR
				# arg1-16*n: colors, comma separated r,g,b
				if len(b) == 1:
					if b[0] == 'GRAYSCALE':
						self.set_palette((DEFPAL_GRAYSCALE,))
					else:
						self.err(self.ERR_UNKNOWN_PALETTE, (b[0],))
				else:
					assert len(b) % 16 == 0
					self.err(self.ERR_NYI)
			else:
				self.err(self.ERR_UNKNOWN_COMMAND, (a,))
			self.scr_lineno += 1

	def set_var(self, vtyp, vnam, vval):
		if vnam in self.SPECIAL_VARS:
			if vnam == 'filelineno':
				assert vtyp == 'int'
				self.fil_linno = self.parsearg(vval)
		else:
			if vtyp == 'str':
				actval = vval
			elif vtyp == 'int':
				actval = self.parsearg(vval)
			else:
				self.err(self.ERR_VALUE, (vtyp,self.cur_cmd,))
			self.var_dict[vnam] = actval

	def pushdata(self, val, typ):
		self.data_stack.append((typ, val))

	def popdata(self, typ):
		for i in range(len(self.data_stack)-1, -1, -1):
			if self.data_stack[i][0] == typ:
				dat = self.data_stack.pop(i)
				return dat[1]
		else:
			self.err(self.ERR_STACK_DATATYPE_NOT_FOUND, (typ,))

	def append_image(self, img, direc):
		if not direc in ('u','d','l','r'):
			self.err(self.ERR_VALUE, (direc,self.cur_cmd,))
		if direc == 'u':
			direc = 'd'
			stor = self.img
			self.img = img
			img = stor
		elif direc == 'l':
			direc = 'r'
			stor = self.img
			self.img = img
			img = stor

		if direc == 'd':
			neww = max(self.img.size[0], img.size[0])
			newh = self.img.size[1] + img.size[1]
			stor = self.img
			self.img = Image.new('RGB', (neww, newh))
			self.img.paste(stor, (0, 0))
			self.img.paste(img, (0, stor.size[1]))
		elif direc == 'r':
			neww = self.img.size[0] + img.size[0]
			newh = max(self.img.size[1], img.size[1])
			stor = self.img
			self.img = Image.new('RGB', (neww, newh))
			self.img.paste(stor, (0, 0))
			self.img.paste(img, (stor.size[0], 0))

	def find_label(self, lbl):
		if not self.fil: self.err(self.ERR_NO_FILE) ; return
		labelre = re.compile(r'.*\b' + lbl + r'\b.*')
		while self.fil_linno < len(self.fil):
			if labelre.match(self.fil[self.fil_linno]):
				return
			self.fil_linno += 1
		self.err(self.ERR_FIND, (lbl,))

	def arr_image(self, tw, th, w2, h2):
		if not self.img: self.err(self.ERR_NO_IMAGE) ; return
		imgout = rearrange(self.img, tw, th, w2, h2)
		self.img = imgout

	def setup_for(self, a, b):
		assert b[0] == '('
		assert b[-1] == ')'
		spl = b[1:-1].split(',')
		assert len(spl) == 2
		lower_val = self.parsearg(spl[0])
		upper_val = self.parsearg(spl[1])
		self.loop_stack.append(['for', a, lower_val, upper_val, self.scr_lineno])
		self.var_dict[a] = lower_val

	def for_done(self):
		loopdat = self.loop_stack[-1]
		loopdat[2] += 1
		if loopdat[2] >= loopdat[3]:
			return
		self.scr_lineno = loopdat[4]
		self.var_dict[loopdat[1]] = loopdat[2]

	def set_palette(self, pals):
		self.pal = pals

	def open_file(self, fn):
		try:
			self.fil = open(fn, encoding='shift-jis').read().split('\n')
		except FileNotFoundError:
			self.fil = None
			self.err(self.ERR_FILE_NOT_FOUND, (fn,))
			return
		self.fil_linno = 0

	def save_image(self, fn):
		if not self.img: self.err(self.ERR_NO_IMAGE) ; return
		self.img.save(fn)

	def get_image(self, numbyt, palnum, w_t):
		if not self.fil: self.err(self.ERR_NO_FILE)    ; return
		if not self.pal: self.err(self.ERR_NO_PALETTE) ; return
		rawimg = self.read_data(numbyt)
		imgout = dat2png(rawimg, self.pal[palnum], w_t)
		self.img = imgout

	def get_compressed_image(self, palnum, w_t):
		if not self.fil: self.err(self.ERR_NO_FILE)    ; return
		if not self.pal: self.err(self.ERR_NO_PALETTE) ; return
		store_pos = (self.fil_linno, self.buf_store)
		rawimg = self.read_data(-1)
		rawimg = self.lz.decomp(rawimg)
		if rawimg == -1:   self.err(self.ERR_LZ_INVALID_DATA) ; return
		elif rawimg == -2: self.err(self.ERR_LZ_DECOMPRESS)   ; return
		rawimg, complen = rawimg
		imgout = dat2png(rawimg, self.pal[palnum], w_t)
		self.img = imgout
		self.fil_linno = store_pos[0]
		self.buf_store = store_pos[1]
		self.read_data(complen)

	def get_palette(self, numpals):
		if not self.fil: self.err(self.ERR_NO_FILE) ; return
		buf = self.read_data(numpals * 0x20)
		self.pal = []
		for i in range(numpals):
			self.pal.append(pal2rgb(buf[i*0x20:i*0x20+0x20]))

	def read_line(self):
		if self.fil_linno >= len(self.fil): return 'EOF'
		l = self.fil[self.fil_linno]
		self.fil_linno += 1
		dat = conv_line(l)
		return dat

	def add_data(self, buf, dat, maxl):
		bufo = buf + dat
		if len(bufo) > maxl:
			ext = bufo[maxl:]
			bufo = bufo[:maxl]
			self.buf_store = ext
		return bufo

	def read_data(self, maxl):
		buf = self.buf_store
		if maxl == -1:
			while True:
				newdat = self.read_line()
				if newdat:
					if newdat == 'EOF':
						break
					else:
						buf += newdat
		else:
			while len(buf) < maxl:
				newdat = self.read_line()
				if newdat:
					if newdat == 'EOF':
						self.err(self.ERR_EOF)
						return
					else:
						buf = self.add_data(buf, newdat, maxl)
		return buf

def Main(fn):
	prs = Parser(fn)
	prs.run()

if __name__ == '__main__':
	if len(sys.argv) < 2:
		print('usage: python3 {} scriptfile'.format(sys.argv[0]))
		exit()
	fn_in = sys.argv[1]
	Main(fn_in)
