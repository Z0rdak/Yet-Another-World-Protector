package de.z0rdak.yawp.mixin.flag.player;

import com.mojang.brigadier.ParseResults;
import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.handler.CommandInterceptor;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static de.z0rdak.yawp.api.events.region.RegionEvents.post;
import static de.z0rdak.yawp.core.flag.RegionFlag.EXECUTE_COMMAND;
import static de.z0rdak.yawp.core.flag.RegionFlag.USE_BONEMEAL;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.getDimKey;
import static de.z0rdak.yawp.handler.flags.HandlerUtil.processCheck;
import static de.z0rdak.yawp.util.MessageSender.sendFlagMsg;

@Mixin(CommandManager.class)
public abstract class CommandManagerMixin {

    @Inject(method = "execute", at = @At(value = "HEAD"), cancellable = true)
    public void execute(ParseResults<ServerCommandSource> parseResults, String command, CallbackInfoReturnable<Integer> cir) {
        // check mod command permissions
        int result = CommandInterceptor.handleModCommands(parseResults, command);
        if (result != 0) {
            cir.setReturnValue(1);
        }
        // check exec-command flag
        ServerCommandSource cmdSource = parseResults.getContext().getSource();
        if (cmdSource.isExecutedByPlayer()) {
            ServerPlayerEntity player = cmdSource.getPlayer();
            if (player != null) {
                FlagCheckEvent checkEvent = new FlagCheckEvent(player.getBlockPos(), EXECUTE_COMMAND, getDimKey(player), player);
                if (post(checkEvent)) {
                    return;
                }
                processCheck(checkEvent, null, deny -> {
                    sendFlagMsg(deny);
                    cir.setReturnValue(1);
                });
            }
        }
    }
}