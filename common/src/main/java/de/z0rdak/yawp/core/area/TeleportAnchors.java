package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import java.util.HashMap;
import java.util.Map;

import static de.z0rdak.yawp.util.ChatComponentBuilder.tinyBlockPos;

public class TeleportAnchors implements INbtSerializable<CompoundTag> {

    public TeleportAnchors() {
        this.tpAnchors = new HashMap<>();
    }

    public TeleportAnchors(CompoundTag tag) {
        this.deserializeNBT(tag);
    }

    protected Map<String, TeleportAnchor> tpAnchors;

    public TeleportAnchor addTpAnchor(BlockPos pos) {
        String derivedName = tinyBlockPos(pos);
        return this.addTpAnchor(pos, derivedName);
    }

    public TeleportAnchor addTpAnchor(BlockPos pos, String name) {
        var anchor = new TeleportAnchor(pos, name);
        this.tpAnchors.put(name, anchor);
        return anchor;
    }

    public void removeTpAnchor(String name) {
        this.tpAnchors.remove(name);
    }

    public void removeTpAnchor(BlockPos pos) {
        this.tpAnchors.remove(tinyBlockPos(pos));
    }

    public TeleportAnchor getTpAnchor(String name) {
        return this.tpAnchors.get(name);
    }

    @Override
    public CompoundTag serializeNBT() {
        return null;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {

    }
}
