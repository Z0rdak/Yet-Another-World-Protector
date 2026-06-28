package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.flag.FlagValue;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;

import java.util.*;
import java.util.stream.Collectors;

public class GlobalRegion extends ProtectedRegion {
    public static final Codec<GlobalRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("active")
                                    .forGetter(ProtectedRegion::isActive),
                            Codec.BOOL.fieldOf("muted")
                                    .forGetter(ProtectedRegion::isMuted),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC).fieldOf("groups")
                                    .forGetter(ProtectedRegion::getGroups),
                            Codec.list(UUIDUtil.STRING_CODEC).fieldOf("childrenIds")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenIds()))
                    )
                    .apply(instance, (flags, isActive, isMuted, groups, childrenIds) ->
                            new GlobalRegion(new RegionFlags(flags), isActive, isMuted, groups, childrenIds)
                    )
    );

    public static final Identifier GLOBAL = Identifier.fromNamespaceAndPath("yawp", "global");
    public static final ResourceKey<Level> GLOBAL_DIMENSION = ResourceKey.create(Registries.DIMENSION, GLOBAL);
    public static final UUID GLOBAL_REGION_UUID = UUID.nameUUIDFromBytes(GLOBAL.toString().getBytes());

    public GlobalRegion() {
        super(GLOBAL.toString(), GLOBAL_REGION_UUID, GLOBAL_REGION_UUID, GLOBAL_DIMENSION,  RegionType.GLOBAL);
    }

    private GlobalRegion(RegionFlags flags, boolean isActive, boolean isMuted, Map<String, PlayerContainer> groups, List<UUID> childrenIds) {
        this();
        this.setFlags(flags);
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenIds(childrenIds);
    }

    @Override
    public Identifier getId() {
        return GLOBAL;
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        Map<String, IProtectedRegion> childrenWithoutGlobal = super.getChildren().entrySet().stream()
                .filter(e -> e.getValue().getRegionType() != RegionType.GLOBAL)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        return Collections.unmodifiableMap(childrenWithoutGlobal);
    }

    @Override
    public Set<UUID> getChildrenIds() {
        return super.getChildren().values().stream()
                .filter(iProtectedRegion -> iProtectedRegion.getRegionType() != RegionType.GLOBAL)
                .map(IProtectedRegion::getUuid)
                .collect(Collectors.toUnmodifiableSet());
    }

}
