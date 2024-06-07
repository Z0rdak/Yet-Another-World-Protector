package de.z0rdak.yawp.core.region;

import net.minecraft.registry.RegistryKeys;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.util.Identifier;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class GlobalRegion extends AbstractRegion {

    public static final Identifier GLOBAL = new Identifier("yawp", "global");
    public static final RegistryKey<World> GLOBAL_DIMENSION = RegistryKey.of(RegistryKeys.WORLD, GLOBAL);

    public GlobalRegion(NbtCompound nbt) {
        super(nbt);
    }

    public GlobalRegion() {
        this(GLOBAL.toString(), RegionType.GLOBAL);
    }

    protected GlobalRegion(String name, RegionType type) {
        super(name, GLOBAL_DIMENSION, type);
        super.setParent(this);
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        Map<String, IProtectedRegion> childrenWithoutGlobal = super.getChildren().entrySet().stream()
                .filter(e -> e.getValue().getRegionType() != RegionType.GLOBAL)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return Collections.unmodifiableMap(childrenWithoutGlobal);
    }

    @Override
    public Set<String> getChildrenNames() {
        Set<String> childrenWithoutGlobal = super.getChildren().values().stream()
                .filter(iProtectedRegion -> iProtectedRegion.getRegionType() != RegionType.GLOBAL)
                .map(IProtectedRegion::getName)
                .collect(Collectors.toSet());
        return Collections.unmodifiableSet(childrenWithoutGlobal);
    }

    @Override
    public boolean setParent(IProtectedRegion parent) {
        if (parent.getRegionType() == RegionType.GLOBAL) {
            return super.setParent(parent);
        }
        return false;
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child.getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        return false;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
    }
}
