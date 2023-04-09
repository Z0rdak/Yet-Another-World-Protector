package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.ParseResults;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.handler.CommandInterceptor;
import de.z0rdak.yawp.handler.flags.FlagCheckEvent;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.handler.flags.HandlerUtil.*;

@Mixin(CommandManager.class)
public abstract class CommandManagerMixin {

    @Shadow
    @Final
    private CommandDispatcher<ServerCommandSource> dispatcher;

    @Inject(method = "execute", at = @At(value = "INVOKE", target = "Lcom/mojang/brigadier/CommandDispatcher;execute(Lcom/mojang/brigadier/StringReader;Ljava/lang/Object;)I"), cancellable = true)
    public void execute(ServerCommandSource commandSource, String command, CallbackInfoReturnable<Integer> cir) {
        // check mod command permissions
        ParseResults<ServerCommandSource> parseResults = this.dispatcher.parse(command, commandSource);
        int result = CommandInterceptor.handleModCommands(parseResults, command);
        if (result != 0) {
            cir.setReturnValue(1);
        }
        // check exec-command flag
        ServerCommandSource cmdSource = parseResults.getContext().getSource();
        try {
            ServerPlayerEntity player = cmdSource.getPlayer();
            if (player != null) {
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(getEntityDim(player));
                FlagCheckEvent.PlayerFlagEvent flagCheck = checkPlayerEvent(player, player.getBlockPos(), RegionFlag.EXECUTE_COMMAND, dimCache.getDimensionalRegion());
                if (flagCheck.isDenied()) {
                    sendFlagDeniedMsg(flagCheck);
                    cir.setReturnValue(1);
                }
            }
        } catch (CommandSyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}