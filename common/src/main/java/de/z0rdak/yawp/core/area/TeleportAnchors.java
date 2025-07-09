package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import java.util.*;

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

    public boolean hasAnchor(String name) {
        return this.tpAnchors.containsKey(name);
    }


    public TeleportAnchor addTpAnchor(BlockPos pos, String name) {
        var anchor = new TeleportAnchor(pos, name);
        this.tpAnchors.put(name, anchor);
        return anchor;
    }

    public void removeTpAnchor(String name) {
        this.tpAnchors.remove(name);
    }

    public void rename(String name, String newName) {
        TeleportAnchor teleportAnchor = this.tpAnchors.get(name);
        this.tpAnchors.remove(name);
        teleportAnchor.setName(newName);
        this.tpAnchors.put(newName, teleportAnchor);
    }

    public void addOrUpdate(String name, BlockPos pos) {
        if (this.tpAnchors.containsKey(name)) {
            TeleportAnchor teleportAnchor = this.tpAnchors.get(name);
            teleportAnchor.setPos(pos);
            return;
        }
        this.addTpAnchor(pos, name);
    }

    public void removeTpAnchor(BlockPos pos) {
        this.tpAnchors.remove(tinyBlockPos(pos));
    }

    public TeleportAnchor getTpAnchor(String name) {
        return this.tpAnchors.get(name);
    }

    public List<TeleportAnchor> getAnchors() {
        return new ArrayList<>(this.tpAnchors.values());
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag tag = new CompoundTag();
        this.tpAnchors.forEach((k, v) -> {
            tag.put(k, v.serializeNBT());
        });
        return tag;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.tpAnchors = new HashMap<>();
        nbt.getAllKeys().forEach((k) -> {
            this.tpAnchors.put(k, new TeleportAnchor(nbt.getCompound(k)));
        });
    }
}
