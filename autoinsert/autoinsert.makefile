# 2007/12/05 18:45:50
# 더 단순한 버전
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
# cmake 가 조잡스러운 경향이 있어서 다시 GNU Make 로 선회.
# 간단한 샘플을 작성하기 위한 Makefile 을 다시 만들어봤다.
#
# OUTPUT_1 에는 출력 프로그램
# SRCS_1 에는 OUTPUT_1 를 빌드하기위한 소스들
# PCH_1 에는 SRCS_1 들이 사용할 precompiled header 를 넣어주면 된다.
#
# 만약 하나의 Makefile 에서 여러개의 바이너를 생성하려면 _1 를 복사해서
# _2 를 만들고 $(OUTPUT_1) 부분을 복사해서 _2 를 만들어 주고.. 등등
# 수작업을 해주자
#
# TODO dep 만드는게 좀 이상한거 같은데..
# TODO pch 헤더의 출력물(*.hpp.gch) 도 builddir 로 옮길까?
# NOTE pch 기능을 넣기는 했는데 버그때문에 쓰기 어렵더라.
#      http://gcc.gnu.org/bugzilla/show_bug.cgi?id=13675 참고


# 아래는 flymake-mode 를 위한 Makefile 코드
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