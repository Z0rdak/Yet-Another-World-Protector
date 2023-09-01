package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

public class FlagMessage implements INBTSerializable<CompoundNBT> {

    public static final String CONFIG_MSG = "config";
    public static final Set<String> MSG_TOKEN;
    public static FlagMessage DEFAULT_FLAG_MSG = new FlagMessage(CONFIG_MSG);

    static {
        MSG_TOKEN = new HashSet<>();
        MSG_TOKEN.add("{flag}");
        MSG_TOKEN.add("{region}");
        MSG_TOKEN.add("{dimension}");
        MSG_TOKEN.add("{pos}");
        MSG_TOKEN.add("{player}");
        MSG_TOKEN.add("{team}");
        MSG_TOKEN.add("{affiliation}");
        MSG_TOKEN.add("{entity}");
        MSG_TOKEN.add("{block}");
    }

    private String msg;
    private boolean muted;
    private boolean isDefault;

    public FlagMessage(String msg) {
        this.msg = msg;
        this.isDefault = msg.toLowerCase(Locale.ROOT).equals(CONFIG_MSG);
    }

    public FlagMessage(String msg, boolean muted) {
        this(msg);
        this.muted = muted;
    }

    public FlagMessage(CompoundNBT msgNbt) {
        this.deserializeNBT(msgNbt);
    }

    public boolean isMuted() {
        return this.muted;
    }

    public void mute(boolean mute) {
        this.muted = mute;
    }

    public void reset() {
        this.isDefault = true;
        this.msg = CONFIG_MSG;
    }

    public boolean isDefault() {
        return this.msg.equals(CONFIG_MSG);
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    @Override
    public String toString() {
        return msg;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(MSG, this.msg);
        nbt.putBoolean(DEFAULT, this.isDefault);
        nbt.putBoolean(MUTED, this.muted);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.msg = nbt.getString(MSG);
        this.muted = nbt.getBoolean(MUTED);
        this.isDefault = nbt.getBoolean(DEFAULT);
    }
}
