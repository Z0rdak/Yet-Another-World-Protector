package de.z0rdak.yawp.api;

import de.z0rdak.yawp.core.flag.FlagMetaInfo;
import net.minecraft.resources.Identifier;

public record Flag(Identifier id, FlagMetaInfo flagInfo) {

    public String name() {
        return id.toString();
    }
}
