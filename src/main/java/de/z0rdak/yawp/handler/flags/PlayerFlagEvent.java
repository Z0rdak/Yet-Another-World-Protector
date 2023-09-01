package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.eventbus.api.Cancelable;

import java.util.Map;

@Cancelable
public class PlayerFlagEvent extends FlagCheckEvent {

    private final PlayerEntity player;
    private Map<String, String> msgSubstitutes;
    public PlayerFlagEvent(PlayerEntity player, DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag flag) {
        super(dimRegion, localRegion, flag);
        this.player = player;
    }

    public PlayerFlagEvent(FlagCheckEvent flagCheckEvent, PlayerEntity player) {
        super(flagCheckEvent.getDimRegion(), flagCheckEvent.getLocalRegion(), flagCheckEvent.getRegionFlag());
        this.player = player;
    }

    public PlayerFlagEvent(FlagCheckEvent flagCheckEvent, PlayerEntity player, Map<String, String> msgSubstitutes) {
        this(flagCheckEvent, player);
        this.msgSubstitutes = msgSubstitutes;
    }

    public PlayerEntity getPlayer() {
        return player;
    }

    public Map<String, String> getMsgSubstitutes() {
        return msgSubstitutes;
    }

}