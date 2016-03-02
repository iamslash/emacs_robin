# 2007/12/05 18:45:50
# �� �ܼ��� ����
CC  = gcc
CXX = g++

all: TAGS a.out

a.out: a.o
	$(CXX) $^ $(LDFLAGS) -o $@

%.o: %.cpp
	$(CXX)  -c $(CXXFLAGS) -o $@ $< -MMD
%.o: %.c
	$(CC)  -c $(CFLAGS) -o $@ $< -MMD

TAGS:
	-@gtags
clean:
	-rm -f *.o
	-rm -f core GPATH GRTAGS GSYMS GTAGS
	-rm -f a.out

-include $(wildcard *.d)

















# 2007/07/23 18:33:06
# 
# cmake �� ���⽺���� ������ �־ �ٽ� GNU Make �� ��ȸ.
# ������ ������ �ۼ��ϱ� ���� Makefile �� �ٽ� �����ô�.
#
# OUTPUT_1 ���� ��� ���α׷�
# SRCS_1 ���� OUTPUT_1 �� �����ϱ����� �ҽ���
# PCH_1 ���� SRCS_1 ���� ����� precompiled header �� �־��ָ� �ȴ�.
#
# ���� �ϳ��� Makefile ���� �������� ���̳ʸ� �����Ϸ��� _1 �� �����ؼ�
# _2 �� ����� $(OUTPUT_1) �κ��� �����ؼ� _2 �� ����� �ְ�.. ���
# ���۾��� ������
#
# TODO dep ����°� �� �̻��Ѱ� ������..
# TODO pch ����� ��¹�(*.hpp.gch) �� builddir �� �ű��?
# NOTE pch ����� �ֱ�� �ߴµ� ���׶����� ���� ��ƴ���.
#      http://gcc.gnu.org/bugzilla/show_bug.cgi?id=13675 ����


# �Ʒ��� flymake-mode �� ���� Makefile �ڵ�
# .PHONY: check-syntax
# check-syntax:
# 	$(CXX) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)



CXX      = g++
BUILDDIR = build
BINDIR   = bin

MAJOR_VERSION = 0
MINOR_VERSION = 0
SVN_REVISION = $(shell svnversion -n)

CXXFLAGS += -Wall -IC:\local\include\boost-1_34
CXXFLAGS += -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DSVN_REVISION=\"$(SVN_REVISION)\"
LDFLAGS  += -LC:\local\lib -lboost_filesystem-mgw34-s-1_34

OUTPUT_1 = $(BINDIR)/findtwins.exe
SRCS_1 = pch.cpp findtwins.cpp walker.cpp
PCH_1  = pch.hpp
PCHOUT_1 = $(PCH_1).gch
OBJS_1 = $(addprefix $(BUILDDIR)/, $(SRCS_1:.cpp=.o))
DEPS_1 = $(OBJS_1:.o=.d)


ifeq ($(release),1)
	CXXFLAGS += -DNDEBUG -O2
else
	CXXFLAGS += -g
endif

ifeq ($(profile),1)
	CXXFLAGS += -pg
endif


.PHONY: clean TAGS strip

all: $(BINDIR) $(BUILDDIR) TAGS $(OUTPUT_1) $(release:1=strip)

$(BINDIR):
	mkdir $(BINDIR)
$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BUILDDIR)/%.o: %.cpp
	@echo COMPILE $<
	@$(CXX) -c $(CXXFLAGS) -o $@ $< -MMD

%.hpp.gch: %.hpp
	@echo PRECOMPILE HEADER $<
	@$(CXX) $(CXXFLAGS) -x c++-header -c $< -o $@

TAGS:
	@echo TAGGING
	-@gtags

$(OUTPUT_1): $(PCHOUT_1) $(OBJS_1)
	@echo LINK $(OUTPUT_1)
	@$(CXX) $(CXXFLAGS) -o $(OUTPUT_1) $(OBJS_1) $(LDFLAGS)

strip:
	-strip $(OUTPUT_1)
	-upx $(OUTPUT_1)

clean:
	-rm -f $(OBJS_1) $(OUTPUT_1) $(DEPS_1) $(PCHOUT_1)
	-rm -f core
	-rm -f GPATH GRTAGS GSYMS GTAGS

-include $(DEPS_1)




# CXX = g++
# CXXFLAGS = -Wall -D_REENTRANT -I/usr/local/include -I/usr/local/include/boost-1_31
# LDFLAGS = -levent -lpthread -lboost_thread-gcc-mt -lsocket -lnsl
# OS_TYPE = $(shell uname -s)

# ifeq ($(release),1)
# 	CXXFLAGS += -DNDEBUG -O3
# else
# 	CXXFLAGS += -g 
# endif

# ifeq ($(profile),1)
# 	CXXFLAGS += -pg
# endif

# ifeq ($(OS_TYPE),Linux)
# 	CXXFLAGS += -D_BUILD_ON_LINUX_
# endif

# ifeq ($(OS_TYPE),SunOS)
# 	CXXFLAGS += -D_BUILD_ON_SUNOS_
# endif

# ifeq ($(OS_TYPE),WindowsNT)
# 	CXXFLAGS += -D_BUILD_ON_WINDOWS_
# endif


# SRCS = $(wildcard *.cpp)
# OBJS = $(SRCS:.cpp=.o)
# DEPS = $(OBJS:.o=.d)
# OUTPUT = a.out

# .PHONY: clean TAGS

# all: TAGS $(OUTPUT)


# %.o: %.cpp
# 	@echo COMPILE $<
# 	@$(CXX) -c $(CXXFLAGS) -o $@ $< -MMD

# # svn_version.cpp:
# # 	@echo SVN repository: `svnversion -n .`
# # 	@printf 'const char* svn_version(void) { const char* SVN_Version = "' > svn_version.cpp
# # 	@svnversion -n . >> svn_version.cpp
# # 	@printf '"; return SVN_Version; }\n' >> svn_version.cpp

# TAGS:
# 	@echo making tags
# 	-@gtags

# $(OUTPUT): $(OBJS)
# 	@echo LINK $(OUTPUT)
# 	@$(CXX) $(CXXFLAGS) -o $(OUTPUT) $(OBJS) $(LDFLAGS)

# clean:
# 	-rm -f $(OBJS) $(OUTPUT) $(DEPS)
# 	-rm -f core
# 	-rm -f GPATH GRTAGS GSYMS GTAGS


# -include $(DEPS)