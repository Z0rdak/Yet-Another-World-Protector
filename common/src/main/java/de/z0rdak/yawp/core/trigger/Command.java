package de.z0rdak.yawp.core.trigger;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;

public record Command(String id, String cmd) {

    public static final Codec<Command> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.STRING.fieldOf("id")
                    .forGetter(Command::id),
            Codec.STRING.fieldOf("cmd")
                    .forGetter(Command::cmd)
    ).apply(instance, Command::new));

}
