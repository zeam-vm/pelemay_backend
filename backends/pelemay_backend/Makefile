.phony: all clean

PRIV = $(MIX_APP_PATH)/priv
BUILD = $(MIX_APP_PATH)/obj
NIF = $(PRIV)/libnif.so

LDFLAGS = -lpthread

ifeq ($(CROSSCOMPILE),)
ifeq ($(shell uname -s),Linux)
LDFLAGS += -fPIC -shared
CFLAGS += -fPIC
else # macOS
LDFLAGS += -undefined dynamic_lookup -dynamiclib -Wl,-w
endif
else
LDFLAGS += -fPIC -shared
CFLAGS += -fPIC
endif

ifeq ($(ERL_EI_INCLUDE_DIR),)
ERLANG_PATH = $(shell elixir --eval ':code.root_dir |> to_string() |> IO.puts')
ifeq ($(ERLANG_PATH),)
$(error Could not find the Elixir installation. Check to see that 'elixir')
endif
ERL_EI_INCLUDE_DIR = $(ERLANG_PATH)/usr/include
ERL_EI_LIBDIR = $(ERLANG_PATH)/usr/lib
endif

ERL_CFLAGS ?= -I$(ERL_EI_INCLUDE_DIR)
ERL_LDFLAGS ?= -L$(ERL_EI_LIBDIR)

CFLAGS += $(shell mix openblas_builder.info including_option)

CFLAGS += -std=c11 -O3 -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-field-initializers

NIF_SRC_DIR = nif_src
C_SRC = $(NIF_SRC_DIR)/libnif.c
C_OBJ = $(C_SRC:$(NIF_SRC_DIR)/%.c=$(BUILD)/%.o)

all: $(PRIV) $(BUILD) $(NIF)

$(PRIV) $(BUILD):
	mkdir -p $@

$(BUILD)/%.o: $(NIF_SRC_DIR)/%.c
	@echo " CC $(notdir $@)"
	$(CC) -c $(ERL_CFLAGS) $(CFLAGS) -o $@ $<

$(NIF): $(C_OBJ) $(OPENBLAS_OBJ)
	@echo " LD $(notdir $@)"
	$(CC) -o $@ $^ $(ERL_LDFLAGS) $(LDFLAGS)

clean:
	$(RM) $(NIF) $(C_OBJ)
