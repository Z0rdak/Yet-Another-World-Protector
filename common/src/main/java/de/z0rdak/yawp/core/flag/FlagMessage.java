package de.z0rdak.yawp.core.flag;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;

import java.util.*;

public class FlagMessage {

    public static final String CONFIG_MSG = "config";
    public static FlagMessage DEFAULT_FLAG_MSG = new FlagMessage(CONFIG_MSG);


    public static Codec<FlagMessage> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.STRING.fieldOf("msg")
                            .orElse(FlagMessage.CONFIG_MSG)
                            .forGetter(FlagMessage::msg),
                    Codec.BOOL.fieldOf("muted")
                            .orElse(false)
                            .forGetter(FlagMessage::isMuted),
                    Codec.BOOL.fieldOf("default")
                            .orElse(true)
                            .forGetter(FlagMessage::isDefault)
            ).apply(instance, FlagMessage::new));

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

    public FlagMessage(String msg, boolean muted, boolean isDefault) {
        this(msg, muted);
        this.isDefault = isDefault;
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
        return this.msg.equals(CONFIG_MSG) || this.isDefault;
    }

    public String msg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    @Override
    public String toString() {
        return msg;
    }
}
