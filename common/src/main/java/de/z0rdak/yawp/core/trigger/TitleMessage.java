package de.z0rdak.yawp.core.trigger;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;

public record TitleMessage(String title, String subTitle, String actionBarTitle, boolean enabled) {

    public static final Codec<TitleMessage> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.STRING.optionalFieldOf("title","region.msg.title")
                    .forGetter(TitleMessage::title),
            Codec.STRING.optionalFieldOf("sub_title", "")
                    .forGetter(TitleMessage::subTitle),
            Codec.STRING.optionalFieldOf("action_bar_title", "")
                    .forGetter(TitleMessage::actionBarTitle),
            Codec.BOOL.optionalFieldOf("enabled", true)
                    .forGetter(TitleMessage::enabled)
    ).apply(instance, TitleMessage::new));

    public static TitleMessage defaultInstance() {
        return new TitleMessage("= &b[%s]&r =", "", "", true);
    }

}
