package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static de.z0rdak.yawp.util.ChatComponentBuilder.tinyBlockPos;

public class RegionAnchors {

    public RegionAnchors() {
        this.tpAnchors = new HashMap<>();
    }

    public RegionAnchors(Map<String, TeleportAnchor> tpAnchors) {
        this.tpAnchors = tpAnchors;
    }

    public Map<String, TeleportAnchor> getTpAnchors() {
        return tpAnchors;
    }

    public void setTpAnchors(Map<String, TeleportAnchor> tpAnchors) {
        this.tpAnchors = tpAnchors;
    }

    protected Map<String, TeleportAnchor> tpAnchors;

    public TeleportAnchor addTpAnchor(BlockPos pos) {
        String derivedName = tinyBlockPos(pos);
        return this.addTpAnchor(pos, derivedName);
    }

    public boolean hasAnchor(String name) {
        return this.tpAnchors.containsKey(name);
    }

    public boolean hasAnchor(String name, BlockPos pos) {
        return this.tpAnchors.containsKey(name)
                && this.tpAnchors.get(name).getPos().equals(pos);
    }

    public boolean hasAnchorWithPos(BlockPos pos) {
        return this.tpAnchors.values().stream()
                .anyMatch(tpAnchor -> tpAnchor.getPos().equals(pos));
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
}
